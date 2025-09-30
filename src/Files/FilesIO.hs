module Files.FilesIO (
    printFileExt,
    flatDirectories,
) where

import Control.Monad (guard, when)
import Data.List.Utils
import GHC.Base (IO (IO))
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, getDirectoryContents)
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
        print $ "path: " <> show path
        print $ "contents: " <> show contents
        directoryContetns $ map (path <>) contents -- тут проблема маппинга пути

data Path a = File a | Dir a

directoryContetns :: [FilePath] -> IO ()
directoryContetns [] = return ()
directoryContetns (path : paths) = do
    let p =
            if path `endswith` "." && path `endswith` ".."
                then Nothing
                else Just path
    mPath <- toPath p
    printFile mPath
    directoryContetns paths

toPath :: Maybe FilePath -> IO (Maybe (Path FilePath))
toPath Nothing = return Nothing
toPath (Just path) = do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    print $ "path: " <> show path
    print $ "isFile: " <> show isFile
    print $ "isDir: " <> show isDir
    if isDir
        then return $ Just $ Dir path
        else
            if isFile
                then return $ Just $ File path
                else return Nothing

printFile :: Maybe (Path FilePath) -> IO ()
printFile Nothing = return ()
printFile (Just (Dir path)) = flatDirectories path
printFile (Just (File path)) = putStrLn path
