module Files.FlatDirectory (
    flatDirectories,
    printFiles,
) where

import Data.List.Utils
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.IO ()

printFiles :: FilePath -> IO ()
printFiles path = flatDirectories path >>= mapM_ putStrLn

data Path a = File a | Dir a | Empty deriving (Show)

{-
    Алгоритм уплощения директорий:
    0. Проверяем все файлы уровня
    1. Проверяем является ли файл директорией
    2. Если да - вход в диреторию, выполнение с п.1
    3. Иначе - переносим все файлы выше на уровень если этот уровень не корневой
    4. Если уровень родительский - ничего не делаем
-}
flatDirectories :: FilePath -> IO [FilePath]
flatDirectories path = do
    contents <- getDirectoryContents path
    directoryContetns $ map (path <>) $ excludeParentDir contents
  where
    excludeParentDir = filter $ not . flip endswith ".."

directoryContetns :: [FilePath] -> IO [FilePath]
directoryContetns [] = return []
directoryContetns (path : paths) = do
    mPath <- toPath path
    filesPaths <- findFilePath mPath
    restPaths <- directoryContetns paths
    return $ filesPaths ++ restPaths

toPath :: FilePath -> IO (Path FilePath)
toPath path = do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isDir
        then return $ Dir $ path <> "/"
        else
            if isFile
                then return $ File path
                else return Empty

findFilePath :: Path FilePath -> IO [FilePath]
findFilePath Empty = return []
findFilePath (Dir path) = flatDirectories path
findFilePath (File path) = return [path]
