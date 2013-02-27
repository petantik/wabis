module Paths_wabis (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/oz/bin"
libdir     = "/home/oz/lib/wabis-0.1.0.0/ghc-7.6.2"
datadir    = "/home/oz/share/wabis-0.1.0.0"
libexecdir = "/home/oz/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "wabis_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wabis_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "wabis_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wabis_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
