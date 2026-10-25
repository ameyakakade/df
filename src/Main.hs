import Parser
import Types
import Stdlib

import Data.Char
import Data.List
import Data.Functor
import Data.Foldable
import Data.Functor.Identity
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.State.Lazy
import Control.Monad.Trans.Either
import System.Environment

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
          <|>   List <$> listParser
          <|>   Str  <$> strParser

programParser :: Parser Atom
programParser = junk *> fmap List (Parser.many1 (atomParser <* junk))

-- Evaluator

inNewScope :: StateP a -> StateP a
inNewScope a = do
    modify $ \s -> s { scope = scope s + 1}
    res <- a
    -- cleanup bindings
    modify $ \s -> s { scope = scope s - 1}
    modify $ \s -> s { definitions = M.map (filter (\(i, _) -> i <= scope s))
                                     (definitions s) }
    modify $ \s -> s { definitions = M.filter (\l -> if null l then False else True) (definitions s) }
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
defineEval :: [Atom] -> StateP Atom
defineEval [n, rs] = case n of
  Str name -> do
      ers <- evaluator rs
      defs <- gets definitions
      defineSymbol name ers
      pure (List [])
  _ -> left "Name has to be a String."

defineEval _ = left "Invalid define block."

-- Deconstructs the lambda using pattern matching, evals the args, and apply.
lambdaEval :: [Atom] -> StateP Atom
lambdaEval ( (List [Str "lambda", List argList, expr] ) :args) = do
    if length argList /= length args
      then left ("Incorrect number of arguments. Expected " ++ show (length argList) ++ ", got " ++ show (length args) ++
                "\n" ++ "Args are: " ++ show args)
      else return (List [])
    inNewScope $ do
        traverse_
          (\(aname, aexpr) -> do
                an <- evaluator aname
                ae <- evaluator aexpr
                case an of
                  Str a -> defineSymbol a ae
                  _ -> left $ "Tried to bind " ++ show an ++ " to " ++ show ae
          )
          (zip argList args)
          -- Add trace every time a lambda is reduced
        modify $ \s ->
          s { trace = trace s ++ ["Definitions: " ++ show (definitions s) ++
                                   "\nExpr: " ++ show expr ++
                                   "\nArgs: " ++ show (zip argList args) ++
                                   "\nScope: " ++ show (scope s) ++ "\n"]}
        evaluator expr

lambdaEval l = left $ "Invalid lambda, " ++ show l

lookupSymbol :: String -> StateP (Maybe Atom)
lookupSymbol name = do
    defs <- gets definitions
    case M.lookup name defs of
      Nothing -> pure Nothing
      -- Find the newest one
      Just set -> pure $ Just $ snd $ foldl' (\(acc, a) (elem, b) -> if acc > elem then (acc, a) else (elem, b)) (0, List []) set

letEval :: [Atom] -> StateP Atom
letEval [List defines, res] = inNewScope $ do
    traverse_ evaluator defines
    evaluator res
letEval _ = left "Invalid syntax for let block."

-- A list when evaluated is function application
listEval :: [Atom] -> StateP Atom
listEval [] = pure (List [])
listEval (a:as) = case a of
  Str s -> case s of
    "define" -> defineEval as
    "if" -> case length as of
              2 -> do
                  let [cond, tb] = as
                  cE <- evaluator cond
                  if cE == List []
                    then pure (List [])
                    else evaluator tb
              3 -> do
                  let [cond, tb, fb] = as
                  cE <- evaluator cond
                  if cE == List []
                    then evaluator fb
                    else evaluator tb
              _ -> left "Invalid if block."
          
    "let" -> letEval as
    s -> do
        d <- lookupSymbol s
        case d of
          Just c -> lambdaEval (c:as)
          Nothing -> case M.lookup s predefined of
                       Just c -> case c of
                                   Unary f -> if length as > 1
                                              then left "Too many arguments, expected 1."
                                              else evaluator (head as) >>= \a -> case f a of
                                                                                   Left m -> left m
                                                                                   Right r -> pure r
                                   Binary f -> if length as > 2
                                               then left "Too many arguments, expected 2."
                                               else do
                                       a1 <- evaluator (head as)
                                       a2 <- evaluator (head (tail as))
                                       case f a1 a2 of
                                         Left m -> left m
                                         Right r -> pure r
                                   Ternary f -> undefined
                       Nothing -> left $ "Unknown function " ++ s
  Num n -> if null as then pure a else left "Invalid function of type 'Num'"
  List (Str "lambda":xs) -> lambdaEval (a:as)
  -- Evaluate the whole list from left to right
  List l -> do
      lr <- listEval l
      listEval (lr:as)
  Quote q -> pure $ List (q:as)

evaluator :: Atom -> StateP Atom
evaluator (List expr) = listEval expr
evaluator (Num a) = pure $ Num a
-- If trying to evaluate a symbol, lookup in defs
evaluator (Str a) = do
    l <- lookupSymbol a
    case l of
      Just a -> pure a
      Nothing -> left ("Could not find variable " ++ a)
evaluator (Quote a) = pure a 
evaluator _ = pure (List []) -- Figure out eval's for other expressions.

main :: IO ()
main = do
    args <- getArgs
    let fn = filter (\s -> not $ isPrefixOf "-" s) args
    let check name = case find (== name) args of Just _ -> True; Nothing -> False
    let debug = check "-debug"
    let putAst = check "-ast"
    let globalSym = check "-symbols"
    code <- fmap concat (traverse readFile fn)
    let (ps, res) = runParser programParser code
    case res of
      Left e -> do
          putStrLn "Syntax error :)"
          print e
      Right (List ast) -> do
          if putAst then print ast else pure ()
          putStrLn ""
          let Identity (res, state) = runStateT (runEitherT (traverse evaluator ast)) initState
          if debug then do putStrLn "Trace: "; traverse_ putStrLn (trace state); putStrLn "" else pure ()
          case res of
            Left a -> do
                putStrLn "Error when interpreting,"
                putStrLn a
            Right a -> do
                putStrLn "Completed evaluation successfully!"
                if globalSym then do putStr "List of global bound symbols: "; print (definitions state) else pure ()
                putStrLn ""
                putStr "Results: "
                print (last a)
                putStr "Output: "
                case M.lookup "output" (definitions state) of
                  Just ((_, Str a):_) -> putStrLn a
                  Just ((_, a):_) -> print a
                  Nothing -> putStrLn "None"
