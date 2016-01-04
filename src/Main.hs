import System.Environment

import Creek

main :: IO ()
main = do
  args <- getArgs
  parse_arguments args

parse_arguments :: [String] -> IO ()
parse_arguments ["-h"] = error "Not implemented"
parse_arguments [path] = do
  obj       <- readFile path
  result    <- return . solve $ read_creek obj
  putStrLn $ show result
    where
      read_creek creek = read creek :: Creek
parse_arguments _         = error "Unknown input, try '-h'"

solve x = error "Not implemented"