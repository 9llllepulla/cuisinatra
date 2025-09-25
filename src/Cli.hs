module Cli (cliParser) where

cliParser :: [String] -> IO ()
cliParser = mapM_ putStrLn
