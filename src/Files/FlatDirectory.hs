module Files.FlatDirectory (
    testExtractFiles,
    testMoveFiles,
    testExtractTypeFiles,
    testMoveTypeFiles,
    Directory,
    getTypeFiles,
    moveTypeFiles,
    removeEmptyDirectoryTree,
) where

import Data.List.Utils (endswith)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, removeDirectoryRecursive, renameFile)
import System.FilePath (takeFileName)
import System.IO ()

------------------------------------------------------------------------------------------------

testExtractFiles :: FilePath -> IO ()
testExtractFiles path = extractFiles path >>= mapM_ putStrLn

testExtractTypeFiles :: FileType -> FilePath -> IO ()
testExtractTypeFiles fType path = getTypeFiles fType (Directory path) >>= mapM_ putStrLn

testMoveFiles :: [FilePath] -> FilePath -> IO ()
testMoveFiles files dir = moveFilesToDirectory files (Directory dir) >>= mapM_ putStrLn

testMoveTypeFiles :: FileType -> FilePath -> FilePath -> IO ()
testMoveTypeFiles fType sourceDir goalDir =
    moveTypeFiles fType (Directory sourceDir) (Directory goalDir) >>= mapM_ putStrLn

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

type FileType = String
newtype Directory = Directory FilePath deriving (Show)
type SourceDirectory = Directory
type GoalDirectory = Directory

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

--
removeEmptyDirectoryTree :: GoalDirectory -> IO (Either String (IO ()))
removeEmptyDirectoryTree rootDir@(Directory dir) = do
    isEmpty <- isEmptyDir rootDir
    if isEmpty
        then return $ Right $ removeDirectoryRecursive dir
        else return $ Left $ "Failed remove directory " <> show rootDir <> ". Directory isn't empty!"

------------------------------------------------------------------------------------------------
isEmptyDir :: Directory -> IO Bool
isEmptyDir dir = undefined -- fixme

-- | Перемещение файлов в новую директорию
moveFilesToDirectory :: [FilePath] -> Directory -> IO [FilePath]
moveFilesToDirectory files (Directory dir) = do
    mapM_ (\(old, new) -> renameFile old new) oldNewPaths
    return $ map snd oldNewPaths
  where
    oldNewPaths = map (\oldName -> (oldName, toNewName oldName)) files
    toNewName file = dir <> takeFileName file

-- | Извлечение файлов из дерева директорий
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

-- | Получение всех файлов дерева директории (со всеми поддиректориями)
getAllFiles :: Directory -> IO [FilePath]
getAllFiles (Directory path) = do
    contents <- getDirectoryContents path
    let fullPaths = map (path <>) $ withoutParentDirContents contents
    mconcat $ map extractFiles fullPaths
  where
    withoutParentDirContents = filter $ not . flip endswith ".."

-- | Фильтрация по типу файла
filesFilter :: FileType -> [FilePath] -> [FilePath]
filesFilter fileType = filter $ \path -> fileType `endswith` path
