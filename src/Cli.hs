module Cli (cliParser) where

{-
	todo Продумать вариант передавать некий предикат группировки файлов
	по разным директориям

-}
cliParser :: [String] -> IO ()
cliParser = mapM_ putStrLn
