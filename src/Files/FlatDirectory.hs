module Files.FlatDirectory (
    testExtractFiles,
    testMoveFiles,
    testExtractTypeFiles,
    extractFiles,
    extractTypeFiles,
) where

import Data.List.Utils (endswith)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, renameFile)
import System.FilePath (takeFileName)
import System.IO ()

testExtractFiles :: FilePath -> IO ()
testExtractFiles path = extractFiles path >>= mapM_ putStrLn

testExtractTypeFiles :: FileType -> FilePath -> IO ()
testExtractTypeFiles fType path = extractTypeFiles fType path >>= mapM_ putStrLn

testMoveFiles :: [FilePath] -> FilePath -> IO ()
testMoveFiles files dir = moveFilesToDirectory files (Directory dir) >>= mapM_ putStrLn

type FileType = String
newtype Directory = Directory FilePath

-- todo добавить перемещение отфильтрованных по типу фалов

extractTypeFiles :: FileType -> FilePath -> IO [FilePath]
extractTypeFiles fType path = do
    files <- extractFiles path
    return $ filesFilter fType files

{- |
    Фильтрация по типу файла
    -- todo заменить на проверку по типу файла
-}
filesFilter :: FileType -> [FilePath] -> [FilePath]
filesFilter fileType = filter $ \path -> fileType `endswith` path

{-
    Алгоритм извлечения файлов из директорий:
    0. Проверяем все файлы уровня
    1. Проверяем является ли файл директорией
    2. Если да - вход в диреторию, выполнение п.0
    3. Иначе - переносим все файлы выше на уровень если этот уровень не корневой
    4. Если уровень родительский - ничего не делаем
-}
extractFiles :: FilePath -> IO [FilePath]
extractFiles path = do
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isDir
        then getAllFiles $ Directory path'
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
getAllFiles :: Directory -> IO [FilePath]
getAllFiles (Directory path) = do
    contents <- getDirectoryContents path
    let fullPaths = map (path <>) $ withoutParentDirContents contents
    mconcat $ map extractFiles fullPaths
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
