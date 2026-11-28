module Main where

import           Evaluator
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
printDefinitions s = void $ M.traverseWithKey (\k v ->
                                           putStr k >> putStr " -> " >>
                                           print (snd $ foldl' (\(acc, a) (elem, b) -> if acc >= elem then (acc, a) else (elem, b)) (-1, Nil) v)
                                          ) (definitions s)

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
