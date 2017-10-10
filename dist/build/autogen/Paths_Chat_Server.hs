{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Chat_Server (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/McGroarty/Library/Haskell/bin"
libdir     = "/Users/McGroarty/Library/Haskell/ghc-8.0.1-x86_64/lib/Chat-Server-0.1.0.0"
datadir    = "/Users/McGroarty/Library/Haskell/share/ghc-8.0.1-x86_64/Chat-Server-0.1.0.0"
libexecdir = "/Users/McGroarty/Library/Haskell/libexec"
sysconfdir = "/Users/McGroarty/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Chat_Server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Chat_Server_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Chat_Server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Chat_Server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Chat_Server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
