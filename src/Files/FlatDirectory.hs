module Files.FlatDirectory (
    flatDirectories,
    printFiles,
) where

import Data.List.Utils
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.IO ()

printFiles :: FilePath -> IO ()
printFiles path = flatDirectories path >>= mapM_ putStrLn

type RootDir = FilePath

{-
    Алгоритм уплощения директорий:
    0. Проверяем все файлы уровня
    1. Проверяем является ли файл директорией
    2. Если да - вход в диреторию, выполнение с п.1
    3. Иначе - переносим все файлы выше на уровень если этот уровень не корневой
    4. Если уровень родительский - ничего не делаем
-}
flatDirectories :: RootDir -> IO [FilePath]
flatDirectories rootDir = do
    rootContents <- getDirectoryContents rootDir
    let contents = map (rootDir <>) $ filterParentDir rootContents
    mconcat $ map toPath contents
  where
    filterParentDir = filter $ not . flip endswith ".."

toPath :: FilePath -> IO [FilePath]
toPath path = do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isDir
        then flatDirectories $ path <> "/"
        else
            if isFile
                then return [path]
                else return []
