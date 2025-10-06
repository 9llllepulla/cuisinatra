module Files.FlatDirectory (
    printFiles,
    moveFiles,
) where

import Data.List.Utils (endswith)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, renameFile)
import System.FilePath (takeFileName)
import System.IO ()

printFiles :: FilePath -> IO ()
printFiles path = extractFilesPaths path >>= mapM_ putStrLn

moveFiles :: [FilePath] -> FilePath -> IO ()
moveFiles paths dir = moveFilesToDirectory paths (DirPath dir)

newtype DirPath = DirPath FilePath

{-
    Алгоритм извлечения файлов из директорий:
    0. Проверяем все файлы уровня
    1. Проверяем является ли файл директорией
    2. Если да - вход в диреторию, выполнение п.0
    3. Иначе - переносим все файлы выше на уровень если этот уровень не корневой
    4. Если уровень родительский - ничего не делаем
-}
extractFilesPaths :: FilePath -> IO [FilePath]
extractFilesPaths path = do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isDir
        then getAllFilesPaths $ DirPath path'
        else
            if isFile
                then return [path]
                else return []
  where
    path' =
        if "/" `endswith` path
            then path
            else path <> "/"

{- |
    Получаем все файлы дерева директории (со всеми поддиректориями)
-}
getAllFilesPaths :: DirPath -> IO [FilePath]
getAllFilesPaths (DirPath path) = do
    contents <- getDirectoryContents path
    let fullPaths = map (path <>) $ getNotParentDirContentsFrom contents
    mconcat $ map extractFilesPaths fullPaths
  where
    getNotParentDirContentsFrom = filter $ not . flip endswith ".."

{-
    todo
    2. Фильтрация по 1 условию получаемых файлов
    3. Заменить проверку родительской директории ".." на проверку пути ?
-}

-- Перемещение файла
-- todo нужно возвращать список файлов
moveFilesToDirectory :: [FilePath] -> DirPath -> IO ()
moveFilesToDirectory paths (DirPath dirPath) = do
    mapM_ (\oldName -> renameFile oldName (newName oldName)) paths
  where
    newName path = dirPath <> takeFileName path
