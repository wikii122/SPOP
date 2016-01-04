import System.Environment

import Board
import Creek

main :: IO ()
main = do
  args  <- getArgs
  parse args

parse :: [String] -> IO ()
parse ["-h"] = undefined
parse [path] = do
  obj       <- readFile path
  result    <- return . solve $ read_creek obj
  putStrLn $ show result
    where
      read_creek creek = read creek :: Creek
parse  _     = error "Unknown input, try '-h'"

solve :: Creek -> Board
solve x = undefined