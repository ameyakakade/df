{-# LANGUAGE DataKinds #-}
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
  _ -> left "Name has to be a String."

defineEval _ = left "Invalid define block."

-- Deconstructs the lambda using pattern matching, evals the args, and apply.
lambdaEval :: Atom -> StateP Atom
lambdaEval (Cons (Cons (Str "lambda") (Cons argList (Cons expr Nil))) args) = do
    if consLen argList /= consLen args
      then left ("Incorrect number of arguments. Expected " ++ show (consLen argList) ++ ", got " ++ show (consLen args) ++
                "\n" ++ "Args are: " ++ show args)
      else return Nil
    inNewScope $ do
        traverse_
          (\(Cons aname aexpr) -> do
                an <- evaluator aname
                ae <- evaluator aexpr
          -- Add trace every time a lambda is reduced
                modify $ \s ->
                  s { trace = trace s ++ ["Definitions: " ++ show (definitions s) ++
                                          "\nExpr: " ++ show expr ++
                                          "\nArgs: " ++ show (consZip argList args) ++
                                          "\nScope: " ++ show (scope s) ++ "\n"]}
                case an of
                  Str a -> defineSymbol a ae
                  _ -> left $ "Tried to bind " ++ show an ++ " to " ++ show ae
          )
          (consToList $ consZip argList args)
        evaluator expr

lambdaEval l = left $ "Invalid lambda, " ++ show l

lookupSymbol :: String -> StateP (Maybe Atom)
lookupSymbol name = do
    defs <- gets definitions
    case M.lookup name defs of
      Nothing -> pure Nothing
      -- Find the newest one
      Just set -> pure $ Just $ snd $ foldl' (\(acc, a) (elem, b) -> if acc >= elem then (acc, a) else (elem, b)) (-1, Nil) set

letEval :: Atom -> StateP Atom
letEval (Cons defines (Cons res Nil)) = inNewScope $ do
    traverse_ evaluator (consToList defines)
    evaluator res
letEval _ = left "Invalid syntax for let block."

-- A list when evaluated is function application
listEval :: Atom -> StateP Atom
listEval Nil = pure Nil
listEval (Cons a as) = case a of
  Str s -> case s of
    "define" -> defineEval as
    "quote" -> pure as
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
              _ -> left "Invalid if block."

    "let" -> letEval as
    s -> do
        d <- lookupSymbol s
        case d of
          Just c -> lambdaEval (Cons c as)
          Nothing -> case M.lookup s predefined of
                       Just c -> case c of
                                   Unary f -> if consLen as /= 1
                                              then left "Incorrect number of arguments, expected 1."
                                              else evaluator (let (Cons a _) = as in a) >>= \a -> case f a of
                                                                                   Left m -> left m
                                                                                   Right r -> pure r
                                   Binary f -> if consLen as /= 2
                                               then left "Incorrect number of arguments, expected 2."
                                               else do
                                       a1 <- evaluator (let (Cons a _) = as in a)
                                       a2 <- evaluator (let (Cons _ (Cons b _)) = as in b)
                                       case f a1 a2 of
                                         Left m  -> left m
                                         Right r -> pure r
                                   Ternary f -> undefined
                       Nothing -> left $ "Unknown function " ++ s
  Num n -> left $ "Invalid function of type 'Num'" ++ show n
  Cons (Str "lambda") _ -> lambdaEval (Cons a as)
  Cons l _ -> left $ "Whatever you are trying, don't."
  Quote q -> left $ "Invalid function \'" ++ show q

evaluator :: Atom -> StateP Atom
evaluator b@(Cons a expr) = listEval b
evaluator (Num a) = pure $ Num a
-- If trying to evaluate a symbol, lookup in defs
evaluator (Str a) = do
    l <- lookupSymbol a
    case l of
      Just a  -> pure a
      Nothing -> left ("Could not find variable " ++ a)
evaluator (Quote a) = pure a
evaluator _ = pure Nil -- Figure out eval's for other expressions.

main :: IO ()
main = do
    args <- getArgs
    let fn = filter (not . isPrefixOf "-") args
    let check name = case find (== name) args of Just _ -> True; Nothing -> False
    let repl      = check "-repl"
    let debug     = check "-trace"
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
                let Identity (res, state) = runStateT (runEitherT (traverse evaluator (consToList ast))) initState
                when debug $ putStrLn "Trace: " >> traverse_ putStrLn (trace state) >> putStrLn ""
                case res of
                  Left a -> do
                      putStrLn "Error when interpreting,"
                      putStrLn a
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
      ":trace"       -> traverse_ putStrLn (trace state) >> replLoop state
      ":help"        -> help >> replLoop state
      _ -> do
          let (ps, res) = runParser programParser inp
          if null (st_str ps) then pure () else putStrLn ("Warning, could not parse: " ++ st_str ps)
          (case res of
             Right ast -> do
                 let Identity (res, nstate) = runStateT (runEitherT (traverse evaluator (consToList ast))) state
                 case res of
                   Left s  -> putStr "Error: " >> putStrLn s >> return state
                   Right s -> traverse_ print s >> return nstate
             Left a -> putStrLn "Syntax error in input, retry." >> return state
            ) >>= replLoop
