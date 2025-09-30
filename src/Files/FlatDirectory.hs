module Files.FlatDirectory (
    flatDirectories,
) where

import Data.List.Utils
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.IO ()

data Path = File FilePath | Dir FilePath

{-
    Алгоритм уплощения директорий:
    0. Проверяем все файлы уровня
    1. Проверяем является ли файл директорией
    2. Если да - вход в диреторию, выполнение с п.1
    3. Иначе - переносим все файлы выше на уровень если этот уровень не корневой
    4. Если уровень родительский - ничего не делаем
-}

flatDirectories :: FilePath -> IO ()
flatDirectories path =
    do
        dirContents <- getDirectoryContents path
        directoryContetns $ map (path <>) $ fromContents dirContents
  where
    fromContents = filter isNotParentDir
    isNotParentDir = not . flip endswith ".."

directoryContetns :: [FilePath] -> IO ()
directoryContetns [] = return ()
directoryContetns (path : paths) = do
    mPath <- toPath path
    printFile mPath
    directoryContetns paths

toPath :: FilePath -> IO (Maybe Path)
toPath path = do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isDir
        then return $ Just $ Dir $ path <> "/"
        else
            if isFile
                then return $ Just $ File path
                else return Nothing

printFile :: Maybe Path -> IO ()
printFile Nothing = return ()
printFile (Just (Dir path)) = flatDirectories path
printFile (Just (File path)) = putStrLn path
