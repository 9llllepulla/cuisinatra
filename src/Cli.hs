module Cli (cliParser) where

{-
	TODO Продумать вариант передавать некий предикат группировки файлов
	по разным директориям

-}
cliParser :: [String] -> IO ()
cliParser = mapM_ putStrLn
