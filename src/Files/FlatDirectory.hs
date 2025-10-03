{-
    Алгоритм уплощения директорий:
    0. Проверяем все файлы уровня
    1. Проверяем является ли файл директорией
    2. Если да - вход в диреторию, выполнение п.0
    3. Иначе - переносим все файлы выше на уровень если этот уровень не корневой
    4. Если уровень родительский - ничего не делаем
-}
module Files.FlatDirectory (
    flatDirectories,
    printFiles,
) where

import Data.List.Utils
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.IO ()

printFiles :: FilePath -> IO ()
printFiles path = flatDirectories path >>= mapM_ putStrLn

type DirPath = FilePath

flatDirectories :: DirPath -> IO [FilePath]
flatDirectories dir = do
    contents <- getDirectoryContents dir
    let fullPathContents = map (dir <>) $ getNotParentDirContentsFrom contents
    mconcat $ map getFilesPaths fullPathContents
  where
    getNotParentDirContentsFrom = filter $ not . flip endswith ".."

getFilesPaths :: FilePath -> IO [FilePath]
getFilesPaths path = do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isDir
        then flatDirectories $ path <> "/"
        else
            if isFile
                then return [path]
                else return []
