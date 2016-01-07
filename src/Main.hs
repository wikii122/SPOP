import System.Environment

import Creek
import Solution

main :: IO ()
main = do
  args    <- getArgs
  result <- parse args
  putStrLn $ result

parse :: [String] -> IO String
parse ["-h"] = return helpText
parse [path] = do
  content <- readFile path
  msolve $ read_creek content
    where
      read_creek creek = read creek :: Creek
      msolve = return . show . solve

parse  _     = error "Unknown input, try '-h'"

helpText :: String
helpText = unlines [
  "usage: spop [PATH]",
  "",
  "This program solves creek stream problem with given input.",
  "To start, you need to create file containg problem description",
  "in supported format and point this executable to it.",
  "",
  "Data file format:",
  "Creek (length, height) [((x, y), no_black_fields)]"]