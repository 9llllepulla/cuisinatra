{- |
    Алгоритм уплощения директорий:
    0. Проверяем все файлы уровня
    1. Проверяем является ли файл директорией
    2. Если да - вход в диреторию, выполнение п.0
    3. Иначе - переносим все файлы выше на уровень если этот уровень не корневой
    4. Если уровень родительский - ничего не делаем
-}
module Files.FlatDirectory (
    printFiles,
) where

import Data.List.Utils (endswith)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.IO ()

-- todo нужно делать через extractFilesPaths
printFiles :: FilePath -> IO ()
printFiles path = extractFilesPaths path >>= mapM_ putStrLn

newtype DirPath = DirPath FilePath

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
