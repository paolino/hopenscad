module Paths_hopenscad (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,2], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/paolino/.cabal/bin"
libdir     = "/home/paolino/.cabal/lib/hopenscad-0.1.2/ghc-7.4.1"
datadir    = "/home/paolino/.cabal/share/hopenscad-0.1.2"
libexecdir = "/home/paolino/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "hopenscad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hopenscad_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hopenscad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hopenscad_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
