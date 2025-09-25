module Main (
    main,
) where

import Cli (cliParser)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    cliParser args
