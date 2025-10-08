module Files.FlatDirectory (
    testPrintFiles,
    testMoveFiles,
) where

import Data.List.Utils (endswith)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, renameFile)
import System.FilePath (takeFileName)
import System.IO ()

testPrintFiles :: FilePath -> IO ()
testPrintFiles path = extractFilesPaths path >>= mapM_ putStrLn

testMoveFiles :: [FilePath] -> FilePath -> IO ()
testMoveFiles files dir = moveFilesToDirectory files (Directory dir) >>= mapM_ putStrLn

newtype Directory = Directory FilePath

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
        then getAllFilesPaths $ Directory path'
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
getAllFilesPaths :: Directory -> IO [FilePath]
getAllFilesPaths (Directory path) = do
    contents <- getDirectoryContents path
    let fullPaths = map (path <>) $ withoutParentDirContents contents
    mconcat $ map extractFilesPaths fullPaths
  where
    withoutParentDirContents = filter $ not . flip endswith ".."

{-
     Перемещение файлов в новую директорию
-}
moveFilesToDirectory :: [FilePath] -> Directory -> IO [FilePath]
moveFilesToDirectory files dir = do
    let oldNewPairs = oldAndNewPaths dir files
    mapM_ (\(old, new) -> renameFile old new) oldNewPairs
    return $ map snd oldNewPairs

oldAndNewPaths :: Directory -> [FilePath] -> [(FilePath, FilePath)]
oldAndNewPaths (Directory dir) = map (\oldName -> (oldName, toNewName oldName))
  where
    toNewName file = dir <> takeFileName file

{-
    todo
    2. Фильтрация по 1 условию получаемых файлов
-}
type FileType = String

filesFilter :: FileType -> [FilePath] -> [FilePath]
filesFilter fileType = filter $ \path -> fileType `endswith` path
