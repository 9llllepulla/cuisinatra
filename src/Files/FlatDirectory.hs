module Files.FlatDirectory (
    testExtractFiles,
    testMoveFiles,
    testExtractTypeFiles,
    testMoveTypeFiles,
    Directory,
    extractFiles,
    extractTypeFiles,
    moveFilesToDirectory,
    moveTypeFilesToDirectory,
) where

import Data.List.Utils (endswith)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, renameFile)
import System.FilePath (takeFileName)
import System.IO ()

------------------------------------------------------------------------------------------------

testExtractFiles :: FilePath -> IO ()
testExtractFiles path = extractFiles path >>= mapM_ putStrLn

testExtractTypeFiles :: FileType -> FilePath -> IO ()
testExtractTypeFiles fType path = extractTypeFiles fType (Directory path) >>= mapM_ putStrLn

testMoveFiles :: [FilePath] -> FilePath -> IO ()
testMoveFiles files dir = moveFilesToDirectory files (Directory dir) >>= mapM_ putStrLn

testMoveTypeFiles :: FileType -> FilePath -> FilePath -> IO ()
testMoveTypeFiles fType sourceDir goalDir =
    moveTypeFilesToDirectory fType (Directory sourceDir) (Directory goalDir) >>= mapM_ putStrLn

------------------------------------------------------------------------------------------------

type FileType = String
newtype Directory = Directory FilePath

{- |
    Извлечение файлов из дерева директорий
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
    Извлечение файлов заданного типа из дерева директорий
-}
extractTypeFiles :: FileType -> Directory -> IO [FilePath]
extractTypeFiles fType (Directory path) = do
    files <- extractFiles path
    return $ filesFilter fType files

{-
     Перемещение файлов в новую директорию
-}
moveFilesToDirectory :: [FilePath] -> Directory -> IO [FilePath]
moveFilesToDirectory files (Directory dir) = do
    mapM_ (\(old, new) -> renameFile old new) oldNewPaths
    return $ map snd oldNewPaths
  where
    oldNewPaths = map (\oldName -> (oldName, toNewName oldName)) files
    toNewName file = dir <> takeFileName file

{-
     Перемещение файлов заданного типа из указанной директории в новую
-}
moveTypeFilesToDirectory :: FileType -> Directory -> Directory -> IO [FilePath]
moveTypeFilesToDirectory fType sourceDir goalDir = do
    files <- getAllFiles sourceDir
    let goalFiles = filesFilter fType files
    moveFilesToDirectory goalFiles goalDir

{- |
    Получение всех файлов дерева директории (со всеми поддиректориями)
-}
getAllFiles :: Directory -> IO [FilePath]
getAllFiles (Directory path) = do
    contents <- getDirectoryContents path
    let fullPaths = map (path <>) $ withoutParentDirContents contents
    mconcat $ map extractFiles fullPaths
  where
    withoutParentDirContents = filter $ not . flip endswith ".."

{- |
    Фильтрация по типу файла
    -- todo заменить на проверку по типу файла
-}
filesFilter :: FileType -> [FilePath] -> [FilePath]
filesFilter fileType = filter $ \path -> fileType `endswith` path
