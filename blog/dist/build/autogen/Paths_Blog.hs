module Paths_Blog (
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
version = Version [0,0,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mbcrocci/.cabal/bin"
libdir     = "/Users/mbcrocci/.cabal/lib/x86_64-osx-ghc-7.10.2/Blog-0.0.0.0-40LWbm12RnIBMpJRVbNbzf"
datadir    = "/Users/mbcrocci/.cabal/share/x86_64-osx-ghc-7.10.2/Blog-0.0.0.0"
libexecdir = "/Users/mbcrocci/.cabal/libexec"
sysconfdir = "/Users/mbcrocci/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Blog_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Blog_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Blog_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Blog_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Blog_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
