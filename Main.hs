{-# LANGUAGE LambdaCase #-}
import Parser

import Data.Char
import qualified Data.Set as E
import Control.Applicative
import Control.Monad.State.Lazy
import System.Environment

data Atom = Num Int
          | Str String
          | Define
          | Let
          | If
          | List [Atom]
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

keywordParser :: Parser Atom
keywordParser = try $ strParser >>=
                \case
                    "define"  -> pure Define
                    "if"      -> pure If
                    "let"     -> pure Let
                    _         -> get >>= \s -> raiseError (Left (FancyError (st_loc s) E.empty))

strParser :: Parser String
strParser = do
    let f c = c /= '(' && c /= ')'
    Parser.many1 (sat (\c -> f c && not (isSpace c)) "Unreachable.")

atomParser :: Parser Atom
atomParser = fmap Quote (char '\'' *> parse) <|> parse
  where parse = Num  <$> numParser
          <|>   List <$> listParser
          <|>   keywordParser
          <|>   Str  <$> strParser

programParser :: Parser [Atom]
programParser = junk *> Parser.many1 (atomParser <* junk)

main :: IO ()
main = do
    [fn] <- getArgs
    code <- readFile fn
    prettyier $ runParser programParser code
    pure ()
