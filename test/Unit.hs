import           System.Directory

import           Printing.PPTypes
import           Printing.PPSubstitution
import           Printing.PPAST

import           Sugar
import           Parser.Parser
import           Common

shouldType :: FilePath -> IO ()
shouldType fp = do
  sourceCode <- readFile fp
  case parseDecs sourceCode >>= checkDecs of
    Left  _ -> putStrLn $ " - " ++ red fp
    Right _ -> putStrLn $ " - " ++ green fp

shouldFail :: FilePath -> IO ()
shouldFail fp = do
  sourceCode <- readFile fp
  case parseDecs sourceCode >>= checkDecs of
    Left  _ -> putStrLn $ " - " ++ green fp
    Right _ -> putStrLn $ " - " ++ red fp

goodPath = "examples/good"
badPath = "examples/bad"

main :: IO ()
main = do
  good <- filter ((/= '.') . head) <$> getDirectoryContents goodPath
  bad  <- filter ((/= '.') . head) <$> getDirectoryContents badPath
  putStrLn "\nTesting good cases:"
  mapM_ shouldType (((goodPath ++ "/") ++) <$> good)
  putStrLn "Testing bad cases:"
  mapM_ shouldFail (((badPath ++ "/") ++) <$> bad)
