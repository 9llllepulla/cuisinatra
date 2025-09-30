module Files.FlatDirectory (
    printFileExt,
    flatDirectories,
) where

import Data.List.Utils
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath (takeExtensions, takeFileName)
import System.IO ()

printFileExt :: FilePath -> IO ()
printFileExt path = do
    let ext = takeExtensions path
    let name = takeFileName path
    putStrLn $ name ++ " ext: " ++ ext

{-
    Алгоритм уплощения директорий:
    0. Проверяем все файлы уровня
    1. Проверяем является ли файл директорией
    2. Если да - вход в диреторию, выполнение с п.1
    3. Иначе - переносим все файлы выше на уровень если этот уровень не корневой
    4. Если уровень корневой ничего не делаем
-}
type DirPath = String

isParentDir :: DirPath -> Bool
isParentDir = flip endswith ".."

flatDirectories :: DirPath -> IO ()
flatDirectories path =
    do
        contents <- getDirectoryContents path
        directoryContetns $ map (path <>) $ filter (not . isParentDir) contents

data Path a = File a | Dir a

directoryContetns :: [FilePath] -> IO ()
directoryContetns [] = return ()
directoryContetns (path : paths) = do
    mPath <- toPath path
    printFile mPath
    directoryContetns paths

toPath :: FilePath -> IO (Maybe (Path FilePath))
toPath path = do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isDir
        then return $ Just $ Dir $ path <> "/"
        else
            if isFile
                then return $ Just $ File path
                else return Nothing

printFile :: Maybe (Path FilePath) -> IO ()
printFile Nothing = return ()
printFile (Just (Dir path)) = flatDirectories path
printFile (Just (File path)) = putStrLn path
