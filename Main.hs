{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
import Parser

import Data.Char
import qualified Data.Set as E
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.State.Lazy
import Control.Monad.Trans.Either
import System.Environment

data Atom = Num Int
          | Str String
          | List [Atom] -- [] is nil or '()
          | Quote Atom
          deriving (Show)

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

data ProgramState = ProgramState {
    definitions :: M.Map String Atom
    } deriving (Show)

initState = ProgramState M.empty

type StateP a = EitherT String (State ProgramState) a

defineEval :: [Atom] -> StateP Atom
defineEval (n:rs) = left "hi"

applyEval :: String -> [Atom] -> StateP Atom
applyEval = undefined

listEval :: [Atom] -> StateP Atom
listEval [] = pure (List [])
listEval (a:as) = case a of
  Str s -> case s of
    "define" -> defineEval as
    "if" -> undefined
    "let" -> undefined
    "lambda" -> undefined
    _ -> applyEval s as
  Num n -> undefined
  List l -> listEval l
  Quote q -> pure q

evaluator :: Atom -> StateP Atom
evaluator (List expr) = listEval expr
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
          print $ runStateT (runEitherT (evaluator ast)) initState

