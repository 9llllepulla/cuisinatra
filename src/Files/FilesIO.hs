module Files.FilesIO (
    printFileExt,
    flatDirectories,
) where

import Control.Monad (guard, when)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (takeExtensions, takeFileName)
import System.IO ()
import System.Posix (fileMode, fileSize, getFileStatus, isDirectory, modificationTime)

printFileExt :: FilePath -> IO ()
printFileExt path = do
    let ext = takeExtensions path
    let name = takeFileName path
    putStrLn $ name ++ " ext: " ++ ext

{-
    Алгоритм рекурсивного уплощения директорий:
    0. Проверяем все файлы уровня
    1. Проверяем является ли файл директорией
    2. Если да - вход в диреторию, выполнение с п.1
    3. Иначе - переносим все файлы выше на уровень если этот уровень не корневой
    4. Если уровень корневой ничего не делаем
-}
type DirPath = String

flatDirectories :: DirPath -> IO ()
flatDirectories path =
    do
        contents <- getDirectoryContents path
        directoryContetns $ filter (\c -> c /= "." && c /= "..") contents

-- mapM_ putStrLn contents

directoryContetns :: [FilePath] -> IO ()
directoryContetns [] = return ()
directoryContetns (path : paths) = undefined

isFile :: FilePath -> Bool
isFile path = undefined

isDir :: FilePath -> Bool
isDir path = undefined
