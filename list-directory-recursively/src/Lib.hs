module Lib
    ( listFilesRecursively
    ) where

import System.Directory
import Control.Monad
import Control.Monad.Extra
import Text.Regex.TDFA

listFilesRecursively :: FilePath -> [Char] -> IO [FilePath]
listFilesRecursively basePath fileNameFilter = do
    let isDirectory = doesDirectoryExist basePath
    let listInternal = (\newBasePath -> do 
                         let filesAndDirectories = listDirectory newBasePath
                         listAllSubfiles newBasePath fileNameFilter =<< filesAndDirectories)

    let listSingleFile = (\path -> do
                           let include = fmap (&& (path =~ fileNameFilter :: Bool)) (doesFileExist basePath)
                           ifM include ((\f -> liftIO [f]) basePath) ((\_ -> liftIO []) basePath))

    ifM isDirectory (listInternal basePath) (listSingleFile basePath)

listAllSubfiles :: FilePath -> [Char] -> [FilePath] -> IO [FilePath]
listAllSubfiles base fileNameFilter = foldl (liftM2 (++)) (liftIO []) . listAllSubfilesInternal base fileNameFilter

listAllSubfilesInternal :: FilePath -> [Char] -> [FilePath] -> [IO [FilePath]]
listAllSubfilesInternal base fileNameFilter = map (\file -> listFilesRecursively (base ++ "/" ++ file) fileNameFilter)

liftIO :: a -> IO a
liftIO = pure
