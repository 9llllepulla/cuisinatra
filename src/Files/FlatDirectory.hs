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
    getDirContetns $ map (rootDir <>) $ filterParentDir rootContents
  where
    filterParentDir = filter $ not . flip endswith ".."

getDirContetns :: [FilePath] -> IO [FilePath]
getDirContetns [] = return []
getDirContetns (path : paths) = do
    mPath <- toPath path
    filesPaths <- findFilesPaths mPath
    restPaths <- getDirContetns paths
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

findFilesPaths :: Path FilePath -> IO [FilePath]
findFilesPaths Empty = return []
findFilesPaths (Dir path) = flatDirectories path
findFilesPaths (File path) = return [path]
