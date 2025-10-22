----------------------------------------------------------------------------------------------
{-
    Модуль работы с директориями:
      - Получение списка файлов из дерева директорий
      - Получение списка файлов заданного типа из дерева директорий
      - Перемещение файлов заданного типа из указанной директории в новую
      - Удаление пустой директории

-}
----------------------------------------------------------------------------------------------
module Files.FlatDirectory (
    Directory (..),
    FileType,
    getTypeFiles,
    moveTypeFiles,
    removeEmptyDirectory,
    extractFiles,
) where

import Data.Functor ((<&>))
import Data.List.Utils (endswith)
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    listDirectory,
    removeDirectory,
    renameFile,
 )
import System.FilePath (takeFileName)
import System.IO ()

------------------------------------------------------------------------------------------------

-- | API

------------------------------------------------------------------------------------------------
type FileType = String

newtype Directory = Directory FilePath deriving (Show)
type SourceDirectory = Directory
type GoalDirectory = Directory

{- |
    Получение списка файлов из дерева директорий
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
    Получение списка файлов заданного типа из дерева директорий
-}
getTypeFiles :: FileType -> Directory -> IO [FilePath]
getTypeFiles fType (Directory path) = do
    files <- extractFiles path
    return $ filesFilter fType files

{- |
     Перемещение файлов заданного типа из указанной директории в новую
-}
moveTypeFiles :: FileType -> SourceDirectory -> GoalDirectory -> IO [FilePath]
moveTypeFiles fType source goal = do
    files <- getAllFiles source
    let goalFiles = filesFilter fType files
    moveFilesToDirectory goalFiles goal

{- |
    Удаление пустой директории
-}
removeEmptyDirectory :: GoalDirectory -> IO (Either String (IO FilePath))
removeEmptyDirectory rootDir@(Directory dir) = do
    isEmpty <- isEmptyDir rootDir
    if isEmpty
        then return $ Right $ removeDirectory dir >> return dir
        else return $ Left $ "Failed remove directory " <> show rootDir <> ". Directory isn't empty!"

------------------------------------------------------------------------------------------------

{- |
    return True если валидный путь, директория существует
    и не содержит файлы или другие директории
-}
isEmptyDir :: Directory -> IO Bool
isEmptyDir (Directory dir) = do
    isExist <- doesPathExist dir
    isDir <- doesDirectoryExist dir
    if isExist && isDir
        then listDirectory dir <&> null
        else return False

-- | Перемещение файлов в новую директорию
moveFilesToDirectory :: [FilePath] -> Directory -> IO [FilePath]
moveFilesToDirectory files (Directory dir) = do
    mapM_ (\(old, new) -> renameFile old new) oldNewPaths
    return $ map snd oldNewPaths
  where
    oldNewPaths = map (\oldName -> (oldName, toNewName oldName)) files
    toNewName file = dir <> takeFileName file

-- | Получение всех файлов дерева директории (со всеми поддиректориями)
getAllFiles :: Directory -> IO [FilePath]
getAllFiles (Directory path) = do
    contents <- listDirectory path
    let fullPaths = map (path <>) contents
    mconcat $ map extractFiles fullPaths

-- | Фильтрация по типу файла
filesFilter :: FileType -> [FilePath] -> [FilePath]
filesFilter fileType = filter $ \path -> fileType `endswith` path
