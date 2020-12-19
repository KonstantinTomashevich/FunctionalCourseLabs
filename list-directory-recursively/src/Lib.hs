module Lib
    ( listFilesRecursively
    ) where

import System.Directory
import Control.Monad
import Control.Monad.Extra
import System.IO.Unsafe
import Text.Regex.TDFA

listFilesRecursively :: FilePath -> [Char] -> IO [FilePath]
listFilesRecursively basePath filter = do
    let isDirectory = doesDirectoryExist basePath
    let listInternal = (\basePath -> do 
                         let filesAndDirectories = listDirectory basePath
                         fmap (listAllSubfiles basePath filter) filesAndDirectories)

    let listSingleFile = (\path -> do
                           let include = fmap (&& (path =~ filter :: Bool)) (doesFileExist basePath)
                           ifM include ((\f -> liftIO [f]) basePath) ((\f -> liftIO []) basePath))

    ifM isDirectory (listInternal basePath) (listSingleFile basePath)

listAllSubfiles :: FilePath -> [Char] -> [FilePath] -> [FilePath]
-- It's safe to use unsafePerformIO here, because results of this function execution will be packed back into IO after this call.
listAllSubfiles base filter files = unsafePerformIO ((foldl (liftM2 (++)) (liftIO []) . listAllSubfilesInternal base filter) files)

listAllSubfilesInternal :: FilePath -> [Char] -> [FilePath] -> [IO [FilePath]]
listAllSubfilesInternal base filter = map (\file -> listFilesRecursively (base ++ "/" ++ file) filter)

liftIO :: a -> IO a
liftIO = pure
