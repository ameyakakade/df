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
defineEval _ = left "Too many arguments for define."

applyEval :: String -> [Atom] -> StateP Atom
applyEval name [] = pure (List [])
applyEval name as = do
    -- If a is a number, error
    -- Lookup a in definitions and replace it
    -- If not found, find a predefined fn.
    defs <- gets definitions
    case M.lookup name defs of
      Just ls -> undefined
      Nothing -> do
          case M.lookup name predefined of
            Nothing -> left ("Invalid function, " ++ name)
            Just f -> case f of
              Unary f -> undefined
              Binary f -> if length as >= 2
                             -- Evaluate the expr before passing it
                             -- along to f
                          then do
                            ea <- evaluator (head as)
                            eas <- evaluator (head (tail as))
                            case f ea eas of
                              Left a -> left a
                              Right a -> listEval (a:drop 2 as)
                          else left "Not enough arguments, expected 2"
    -- Evaluate a, if a is a lambda, get first element from as

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
    _ -> applyEval s as
  Num n -> if null as then pure a else left "Invalid function of type 'Num'"
  -- Evaluate the whole list from left to right
  List l -> do
      l <- listEval l
      ls <- traverse evaluator as
      pure $ List (l:ls)
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
      Right ast -> do
          print ast
          putStrLn ""
          let Identity (res, state) = runStateT (runEitherT (evaluator ast)) initState
          case res of
            Left a -> do
                putStrLn "Error when interpreting,"
                print a
            Right a -> do
                putStrLn "Completed evaluation successfully!"
                putStr "List of global bound symbols: "
                print state
                let (List l) = a
                putStr "Results: "
                print l
