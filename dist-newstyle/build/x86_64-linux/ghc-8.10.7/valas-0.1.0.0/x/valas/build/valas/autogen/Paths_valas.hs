{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_valas (
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

bindir     = "/home/danielhome1/.cabal/bin"
libdir     = "/home/danielhome1/.cabal/lib/x86_64-linux-ghc-8.10.7/valas-0.1.0.0-inplace-valas"
dynlibdir  = "/home/danielhome1/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/danielhome1/.cabal/share/x86_64-linux-ghc-8.10.7/valas-0.1.0.0"
libexecdir = "/home/danielhome1/.cabal/libexec/x86_64-linux-ghc-8.10.7/valas-0.1.0.0"
sysconfdir = "/home/danielhome1/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "valas_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "valas_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "valas_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "valas_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "valas_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "valas_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
