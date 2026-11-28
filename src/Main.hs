import           Prelude hiding (error)
import           Parser
import           Stdlib
import           Types

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.State.Lazy
import           Control.Monad.Trans.Either
import           Data.Char
import           Data.Foldable
import           Data.Functor
import           Data.Functor.Identity
import           Data.List
import qualified Data.Map                   as M
import           System.Environment
import           System.Exit
import           System.IO

comment :: Parser ()
comment = do
  string ";"
  Parser.many (sat (/= '\n') "Expected newline.")
  return ()

junk :: Parser ()
junk = do
  Parser.many (spaces <|> comment)
  return ()

listParser :: Parser [Atom]
listParser = do
    char '('
    junk
    r <- Parser.many (atomParser <* junk)
    char ')'
    junk
    pure r

numParser :: Parser Int
numParser = read <$> many1 (sat isNumber "Expected a number.")

strParser :: Parser String
strParser = do
    let f c = c /= '(' && c /= ')'
    Parser.many1 (sat (\c -> f c && not (isSpace c)) "Unreachable.")

atomParser :: Parser Atom
atomParser = fmap Quote (char '\'' *> parse) <|> parse
  where parse = Num  <$> numParser
          <|>   listToCons <$> listParser
          <|>   Str  <$> strParser

programParser :: Parser Atom
programParser = junk *> fmap listToCons (Parser.many (atomParser <* junk)) <* junk

-- Evaluator

inNewScope :: StateP a -> StateP a
inNewScope a = do
    modify $ \s -> s { scope = scope s + 1}
    res <- a
    -- cleanup bindings
    modify $ \s -> s { scope = scope s - 1}
    modify $ \s -> s { definitions = M.map (filter (\(i, _) -> i <= scope s))
                                     (definitions s) }
    modify $ \s -> s { definitions = M.filter (not . null) (definitions s) }
    pure res

defineSymbol :: String -> Atom -> StateP ()
defineSymbol name ers = do
    defs <- gets definitions
    case M.lookup name defs of
        -- No previous variables
      Nothing -> modify (\s -> s { definitions = M.insert name [(scope s, ers)] (definitions s) })
      -- If shadowing and symbol is bound to a singleton, if the new value is same, just keep the old value.
      -- This should work even if the list has other elements
      Just [a] -> if snd a == ers then return ()
                  else modify (\s -> s { definitions = M.insert name ((:) (scope s, ers) [a]) (definitions s) })
      Just a -> modify (\s -> s { definitions = M.insert name ((:) (scope s, ers) a) (definitions s) })

-- Define and binds a expr
defineEval :: Atom -> StateP Atom
defineEval (Cons n (Cons rs Nil)) = case n of
  Str name -> do
      ers <- evaluator rs
      defs <- gets definitions
      defineSymbol name ers
      pure Nil
  _ -> error "Name has to be a String."

defineEval _ = error "Invalid define block."

lambdaEval = tracer lambdaEvalH

-- Deconstructs the lambda using pattern matching, evals the args, and apply.
lambdaEvalH :: Atom -> StateP Atom
lambdaEvalH (Cons (Cons (Str "lambda") (Cons argList (Cons expr Nil))) args) = do
    if consLen argList /= consLen args
      then error ("Incorrect number of arguments. Expected " ++ show (consLen argList) ++ ", got " ++ show (consLen args) ++
                "\n" ++ "Args are: " ++ show args)
      else return Nil
    inNewScope $ do
        traverse_
          (\(Cons aname aexpr) -> do
                let an = aname
                ae <- evaluator aexpr
                case an of
                  Str a -> defineSymbol a ae
                  _ -> error $ "Tried to bind " ++ show an ++ " to " ++ show ae
          )
          (consToList $ consZip argList args)
        evaluator expr

lambdaEvalH l = error $ "Invalid lambda, " ++ show l

macroEval = tracer macroEvalH

-- Deconstructs the macro using pattern matching, do NOT eval the args just bind, and return expr.
macroEvalH :: Atom -> StateP Atom
macroEvalH (Cons (Cons (Str "macro") (Cons argList (Cons expr Nil))) args) = do
    inNewScope $ do
        traverse_
          (\(Cons aname aexpr) -> do
                let an = aname
                let ae = aexpr
                case an of
                  Str a -> defineSymbol a ae
                  _ -> error $ "Tried to bind " ++ show an ++ " to " ++ show ae
          )
          (consToList $ consZip argList args)
        evaluator expr

macroEvalH a = error (show a)

lookupSymbol :: String -> StateP (Maybe Atom)
lookupSymbol name = do
    defs <- gets definitions
    case M.lookup name defs of
      Nothing -> pure Nothing
      -- Find the newest one
      Just set -> pure $ Just $ snd $ foldl' (\(acc, a) (elem, b) -> if acc >= elem then (acc, a) else (elem, b)) (-1, Nil) set

letEval = tracer letEvalH

letEvalH :: Atom -> StateP Atom
letEvalH (Cons defines (Cons res Nil)) = inNewScope $ do
    traverse_ evaluator (consToList defines)
    evaluator res
letEvalH _ = error "Invalid syntax for let block."

listEval = tracer listEvalH

-- A list when evaluated is function application
listEvalH :: Atom -> StateP Atom
listEvalH Nil = pure Nil
listEvalH (Cons a as) = case a of
  Str s -> case s of
    "define" -> defineEval as
    "quote" -> case as of
                 Cons a Nil -> pure a
                 _ -> error "Unexpected number of arguments for quote."
    "if" -> case consLen as of
              2 -> do
                  let [cond, tb] = consToList as
                  cE <- evaluator cond
                  if cE == Nil
                    then pure Nil

                    else evaluator tb
              3 -> do
                  let [cond, tb, fb] = consToList as
                  cE <- evaluator cond
                  if cE == Nil
                    then evaluator fb
                    else evaluator tb
              _ -> error "Invalid if block."

    "let" -> letEval as
    "eval" -> case as of
                Cons expr Nil -> evaluator expr >>= evaluator
                _ -> error "Unexpected number of arguments for eval."
    -- This is the worst code in this project and must be changed
    s -> do
        d <- lookupSymbol s
        case d of
          Just c@(Cons (Str "lambda") _) -> lambdaEval (Cons c as)
          Just c@(Cons (Str "macro") _) -> macroEval (Cons c as) >>= evaluator
          Just c -> error $ "What? -> " ++ show c ++ show s
          Nothing -> case M.lookup s predefined of
                       Just c -> case c of
                                   Unary f -> if consLen as /= 1
                                              then error "Incorrect number of arguments, expected 1."
                                              else let (Cons a _) = as in evaluator a >>= f
                                   Binary f -> if consLen as /= 2
                                               then error "Incorrect number of arguments, expected 2."
                                               else do
                                       a1 <- evaluator (let (Cons a _) = as in a)
                                       a2 <- evaluator (let (Cons _ (Cons b _)) = as in b)
                                       f a1 a2 
                                   Ternary f -> undefined
                       Nothing -> error $ "Unknown function " ++ s
  Num n -> error $ "Invalid function of type 'Num'" ++ show n
  Cons (Str "macro") _ -> error "HEIFHO"
  Cons (Str "lambda") _ -> lambdaEval (Cons a as)
  Cons l ls -> error $ "Whatever you are trying, don't. -> " ++ show (Cons l ls)
  Quote q -> error $ "Invalid function \'" ++ show q

-- Evaluator will NOT make a new scope
evaluator :: Atom -> StateP Atom
evaluator b@(Cons a expr) = listEval b
evaluator (Num a) = tracer pure (Num a)
-- If trying to evaluate a symbol, lookup in defs
evaluator (Str a) = do
    l <- lookupSymbol a
    case l of
      Just a  -> pure a
      Nothing -> error ("Could not find variable " ++ a)
evaluator (Quote a) = tracer pure a
evaluator _ = pure Nil -- Figure out eval's for other expressions.

-- Every eval function goes through this, it will keep a 'stack trace'
-- and can be used to make a step debugger.
tracer :: (Atom -> StateP Atom) -> Atom -> StateP Atom
tracer fn expr = gets traceFlag >>= \a ->
  case a of
    None -> fn expr
    Trace -> do
        modify (\s -> s { trace = expr:(trace s) })
        r <- fn expr
        modify (\s -> s { trace = tail (trace s) })
        return r
    StepIn -> do
        modify (\s -> s { trace = expr:(trace s) })
        r <- fn expr
        t <- gets trace
        liftIO $ do
            putStr " -> "
            print r
            traverse_ (\a -> print a >> putStrLn "") (take 7 t)
        modify (\s -> s { trace = tail (trace s) })
        return r

error :: String -> StateP a
error a = gets traceFlag
  >>= \l -> case l of
              None -> do
                  left $ a ++ "\nUse '-trace' to get stack dump."
              Trace -> do
                  gets trace >>= \t -> left $ a ++ "\nTrace: \n" ++ (unlines (map show t))

main :: IO ()
main = do
    args <- getArgs
    let fn = filter (not . isPrefixOf "-") args
    let check name = case find (== name) args of Just _ -> True; Nothing -> False
    let repl      = check "-repl"
    let trace     = check "-trace"
    let traceAll  = check "-traceall"
    let putAst    = check "-ast"
    let globalSym = check "-definitions"
    code <- Control.Exception.try $ fmap concat (traverse readFile fn) :: IO (Either IOException String)
    case code of
      Left e -> do putStrLn $ "Could not open file: " ++ show e; exitWith (ExitFailure 1)
      Right code -> do
          let (ps, res) = runParser programParser code
          if null (st_str ps) then return () else putStrLn $ "Unexpected string at end:\n " ++ show (st_str ps)
          case res of
            Left e -> do
                putStrLn "Syntax error :)"
                print e
                exitWith (ExitFailure 1)
            Right ast -> do
                when putAst (print ast)
                putStrLn ""
                (res, state) <- runStateT (runEitherT (traverse evaluator (consToList ast)))
                                (initState { traceFlag = if trace || traceAll then Trace else None })
                case res of
                  Left a -> do
                      putStrLn "Error when interpreting,"
                      if traceAll
                        then do
                          putStrLn a
                        else do
                          -- TODO: Assuming error message is 2 lines and rest is trace.
                          putStrLn $ unlines (let l = lines a
                                              in if length l > 10
                                                 then take 6 l ++ ["..."] ++ drop (length l - 4) l
                                                 else l
                                             )
                          putStrLn $ "Pass '-traceall' to show full trace."
                      exitWith (ExitFailure 1)
                  Right a -> do
                      putStrLn "Completed evaluation successfully!"
                      when globalSym $ putStrLn "Globally bound symbols: " >> printDefinitions state
                      putStrLn ""
                      putStr "Results: "
                      if null a then putStrLn "nil" else print (last a)
                      putStr "Output: "
                      case M.lookup "output" (definitions state) of
                        Just ((_, Str a):_) -> putStrLn a
                        Just ((_, b@(Cons _ _)):_) -> traverse_ print (consToList b)
                        Just ((_, a):_)     -> print a
                        Nothing             -> putStrLn "None"
                      when repl
                        (do
                          putStrLn ""
                          help
                          putStrLn "Entering repl:"
                          replLoop state
                          putStrLn "Quitting.")

help :: IO ()
help = putStrLn "Help"
       >> putStrLn ":q or :quit  - exit"
       >> putStrLn ":clear       - remove all definitions and trace"
       >> putStrLn ":definitions - display all bounded symbols"
       >> putStrLn ":trace       - display (unreadable) trace"
       >> putStrLn ":help        - display this message"

printDefinitions :: ProgramState -> IO ()
printDefinitions s = M.traverseWithKey (\k v ->
                                           putStr k >> putStr " -> " >>
                                           print (snd $ foldl' (\(acc, a) (elem, b) -> if acc >= elem then (acc, a) else (elem, b)) (-1, Nil) v)
                                          ) (definitions s) >> pure ()

replLoop :: ProgramState -> IO ()
replLoop state = do
    putStr "λ> "
    hFlush stdout
    inp <- getLine
    case inp of
      ":q"           -> pure ()
      ":quit"        -> pure ()
      ":clear"       -> putStrLn "Clearing all definitions" >> replLoop initState
      ":definitions" -> printDefinitions state >> replLoop state
      ":trace"       -> traverse_ print (trace state) >> replLoop state
      ":help"        -> help >> replLoop state
      _ -> do
          let (ps, res) = runParser programParser inp
          if null (st_str ps) then pure () else putStrLn ("Warning, could not parse: " ++ st_str ps)
          (case res of
             Right ast -> do
                 (res, nstate) <- runStateT (runEitherT (traverse evaluator (consToList ast))) state
                 case res of
                   Left s  -> putStr "Error: " >> putStrLn s >> return state
                   Right s -> traverse_ print s >> return nstate
             Left a -> putStrLn "Syntax error in input, retry." >> return state
            ) >>= replLoop
