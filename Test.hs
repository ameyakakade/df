import Data.Function
import Data.List
import Data.String
import Data.Foldable
import Control.Monad
import Control.Exception
import Control.Concurrent
import System.Process
import System.Environment
import System.Directory
import System.IO

data Op = Test | Reset | Erase

main :: IO ()
main = do
    args <- getArgs
    let a = filter (not . isPrefixOf "-") args
    let check name = case find (== name) args of Just _ -> True; Nothing -> False
    let flag = if check "-reset"
               then Reset
               else
                 if check "-erase"
                 then Erase
                 else Test
    case a of
      [] -> putStrLn "No files provided, testing all files in 'examples/'"
            >> getCurrentDirectory
            >>= listDirectory . (++ "/examples")
            >>= traverseP (testFile flag) . filter (isSuffixOf ".df") . map ("examples/" ++)
      a -> traverseP (testFile flag) a

traverseP :: (a -> IO b) -> [a] -> IO ()
traverseP ma ls = do
    endFlags <- newChan
    traverse_ (\fp -> forkIO (soldier endFlags (ma fp))) ls
    mapM_ (const $ readChan endFlags) (replicate (length ls) ())

soldier :: Chan () -> IO a -> IO ()
soldier ch mai = mai >> writeChan ch ()

testFile :: Op -> FilePath -> IO ()
testFile Erase fp = do
    fileContent <- readFile' fp
    putStrLn ("Erasing test data for: " ++ fp)
    writeFile fp (unlines (takeWhile (not . isInfixOf "-- TEST BLOCK --") (lines fileContent)))

testFile reset fp = do
    fileContent <- readFile' fp
    op <- runFile fp
    case op of
      Left err -> putStrLn ("Error when interpreting this file, please fix.") >> print err
      Right op -> 
        case (seperate fileContent, reset) of
          (_, Reset) -> do
              putStrLn ("Erasing and writing test data for: " ++ fp)
              writeFile fp (unlines (takeWhile (not . isInfixOf "-- TEST BLOCK --") (lines fileContent)) ++ createTestBlock op)
          (Just test, _) -> do
              if op == test
                then putStrLn ("Test passed successfully: " ++ fp)
                else putStrLn ("Test FAILED for: " ++ fp)
                     >> putStrLn ("Expected: \n" ++ op ++ "\nGot: \n" ++ test)
          (Nothing, _) -> do
              putStrLn ("Writing test data for: " ++ fp)
              writeFile fp (fileContent ++ createTestBlock op)

runFile :: FilePath -> IO (Either IOError String)
runFile fp = try $ readProcess "cabal" ["exec", "df", "--", fp] ""

seperate :: String -> Maybe String
seperate file = let a = dropWhile (not . isInfixOf "-- TEST BLOCK --") (lines file)
                in if null a then Nothing else Just (unlines $ drop 1 <$> drop 1 a)

createTestBlock :: String -> String
createTestBlock = unlines . (";-- TEST BLOCK --":) . map (';':) . lines 
