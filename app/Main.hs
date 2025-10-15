module Main (
    main,
) where

import Cli (cliParser)
import Files.FlatDirectory (testExtractFiles)
import System.Environment (getArgs)

main :: IO ()
main = do
    testExtractFiles "/home/gllllepulla/HaskellWorkSpace/test-content"

{-args <- getArgs
cliParser args-}
