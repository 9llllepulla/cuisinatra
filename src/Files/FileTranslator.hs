module Files.FileTranslator (
    printFileExt,
)
where

import System.FilePath (takeExtensions, takeFileName)

-- stub
printFileExt :: FilePath -> IO ()
printFileExt path = do
    let ext = takeExtensions path
    let name = takeFileName path
    putStrLn $ name ++ " ext: " ++ ext

{-
    todo
    - группировка файлов по типу по директориям
    - группировка файлов типа jpeg по параметрам exif по директориям
-}
