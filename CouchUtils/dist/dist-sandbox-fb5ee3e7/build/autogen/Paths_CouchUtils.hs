module Paths_CouchUtils (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Erwan/projects/haskell/dbTest/.cabal-sandbox/bin"
libdir     = "/Users/Erwan/projects/haskell/dbTest/.cabal-sandbox/lib/x86_64-osx-ghc-7.6.3/CouchUtils-0.1.0.0"
datadir    = "/Users/Erwan/projects/haskell/dbTest/.cabal-sandbox/share/x86_64-osx-ghc-7.6.3/CouchUtils-0.1.0.0"
libexecdir = "/Users/Erwan/projects/haskell/dbTest/.cabal-sandbox/libexec"
sysconfdir = "/Users/Erwan/projects/haskell/dbTest/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CouchUtils_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CouchUtils_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "CouchUtils_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CouchUtils_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CouchUtils_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
