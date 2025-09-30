module Files.FileTranslator (
    printFileExt,
)
where

import System.FilePath (takeExtensions, takeFileName)

printFileExt :: FilePath -> IO ()
printFileExt path = do
    let ext = takeExtensions path
    let name = takeFileName path
    putStrLn $ name ++ " ext: " ++ ext
