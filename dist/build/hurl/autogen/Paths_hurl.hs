{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hurl (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jacoby6000/.cabal/bin"
libdir     = "/home/jacoby6000/.cabal/lib/x86_64-linux-ghc-8.2.2/hurl-0.1.0.0-LPIksRbfkgRK4plZ7CDEAd-hurl"
dynlibdir  = "/home/jacoby6000/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/jacoby6000/.cabal/share/x86_64-linux-ghc-8.2.2/hurl-0.1.0.0"
libexecdir = "/home/jacoby6000/.cabal/libexec/x86_64-linux-ghc-8.2.2/hurl-0.1.0.0"
sysconfdir = "/home/jacoby6000/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hurl_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hurl_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hurl_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hurl_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hurl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hurl_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
