module Paths_mangaServer (
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

bindir     = "/Users/Erwan/projects/haskell/mangaServer/.cabal-sandbox/bin"
libdir     = "/Users/Erwan/projects/haskell/mangaServer/.cabal-sandbox/lib/x86_64-osx-ghc-7.6.3/mangaServer-0.1.0.0"
datadir    = "/Users/Erwan/projects/haskell/mangaServer/.cabal-sandbox/share/x86_64-osx-ghc-7.6.3/mangaServer-0.1.0.0"
libexecdir = "/Users/Erwan/projects/haskell/mangaServer/.cabal-sandbox/libexec"
sysconfdir = "/Users/Erwan/projects/haskell/mangaServer/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mangaServer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mangaServer_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mangaServer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mangaServer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mangaServer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
