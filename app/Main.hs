module Main (
    main,
    testExtractFiles,
    testExtractTypeFiles,
    testMoveTypeFiles,
    testRemoveEmptyDir,
) where

import Cli (cliParser)
import Files.FlatDirectory
import System.Environment (getArgs)

main :: IO ()
main = do
    testExtractFiles "/home/gllllepulla/HaskellWorkSpace/test-content"

{-args <- getArgs
cliParser args-}

testExtractFiles :: FilePath -> IO ()
testExtractFiles path = extractFiles path >>= mapM_ putStrLn

testExtractTypeFiles :: FileType -> FilePath -> IO ()
testExtractTypeFiles fType path = getTypeFiles fType (Directory path) >>= mapM_ putStrLn

testMoveTypeFiles :: FileType -> FilePath -> FilePath -> IO ()
testMoveTypeFiles fType sourceDir goalDir =
    moveTypeFiles fType (Directory sourceDir) (Directory goalDir) >>= mapM_ putStrLn

testRemoveEmptyDir :: FilePath -> IO ()
testRemoveEmptyDir path = do
    dirOrError <- removeEmptyDirectory (Directory path)
    printThis dirOrError
  where
    printThis (Right dir) = dir >>= putStrLn
    printThis (Left msg) = putStrLn msg
