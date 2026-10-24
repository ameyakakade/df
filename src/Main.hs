import Parser
import Types
import Stdlib

import Data.Char
import Data.Functor
import Data.Functor.Identity
import qualified Data.Set as E
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

-- Stores map of string to scope no and atom
-- Stores current scope index
data ProgramState = ProgramState {
    definitions :: M.Map String (Int, Atom),
    scope :: Int
    } deriving (Show)

initState = ProgramState M.empty 0

type StateP a = EitherT String (State ProgramState) a

inNewScope :: StateP a -> StateP a
inNewScope a = do
    modify $ \s -> s { scope = (scope s) + 1}
    res <- a
    -- cleanup bindings
    modify $ \s -> s { scope = (scope s) - 1}
    modify $ \s -> s { definitions = M.filter (\(i, _) -> i <= scope s) (definitions s) }
    pure res

-- Define and binds a expr
defineEval :: [Atom] -> StateP Atom
defineEval [n, rs] = case n of
  Str name -> do
      ers <- evaluator rs
      index <- gets scope
      modify (\s -> s { definitions = M.insert name (index, ers) (definitions s) }) $> List []
  _ -> left "Name has to be a String."
defineEval _ = left "Invalid define block."

-- Deconstructs the lambda using pattern matching, evals the args, and apply.
lambdaEval :: [Atom] -> StateP Atom
lambdaEval ( (List [Str "lambda", (List argList), expr] ) :args) = do
    if length argList /= length args
      then left ("Incorrect number of arguments. Expected " ++ show (length argList) ++ ", got " ++ show (length args))
      else return (List [])
    inNewScope $ do
        traverse
          (\(aname, aexpr) -> do
                an <- evaluator aname
                ae <- evaluator aexpr
                case an of
                  Str a -> modify (\s -> s { definitions = M.insert a (scope s, ae) (definitions s) })
                  _ -> left $ "Tried to bind " ++ show an ++ " to " ++ show ae
          )
          (zip argList args)
        evaluator expr

lambdaEval l = left $ "Invalid lambda, " ++ show l

lookupSymbol :: String -> StateP (Maybe Atom)
lookupSymbol name = do
    defs <- gets definitions
    pure $ fmap snd $ M.lookup name defs

letEval :: [Atom] -> StateP Atom
letEval [(List defines), res] = inNewScope $ do
    traverse evaluator defines
    evaluator res
letEval _ = left "Invalid syntax for let block."

-- A list when evaluated is function application
listEval :: [Atom] -> StateP Atom
listEval [] = pure (List [])
listEval (a:as) = case a of
  Str s -> case s of
    "define" -> defineEval as
    "if" -> undefined
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
                       Nothing -> left ("Unknown function " ++ s)
  Num n -> if null as then pure a else left "Invalid function of type 'Num'"
  -- Evaluate the whole list from left to right
  List l -> do
      lr <- listEval l
      listEval (lr:as)
  Quote q -> pure q

evaluator :: Atom -> StateP Atom
evaluator (List expr) = listEval expr
evaluator (Num a) = pure $ Num a
-- If trying to evaluate a symbol, lookup in defs
evaluator (Str a) = do
    defs <- gets definitions
    case M.lookup a defs of
      Nothing -> left ("Could not find variable, " ++ a)
      Just res -> pure $ snd res
evaluator (Quote a) = pure $ a 
evaluator _ = pure (List []) -- Figure out eval's for other expressions.

main :: IO ()
main = do
    [fn] <- getArgs
    code <- readFile fn
    let (ps, res) = runParser programParser code
    case res of
      Left e -> do
          putStrLn "Syntax error :)"
          print e
      Right (List ast) -> do
          print ast
          putStrLn ""
          let Identity (res, state) = runStateT (runEitherT (traverse evaluator ast)) initState
          case res of
            Left a -> do
                putStrLn "Error when interpreting,"
                putStrLn a
            Right a -> do
                putStrLn "Completed evaluation successfully!"
                putStr "List of global bound symbols: "
                print state
                putStr "Results: "
                print a
