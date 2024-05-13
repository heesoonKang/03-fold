{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hw3_fold (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/workspaces/03-fold/.stack-work/install/x86_64-linux/f8297707d067d3a8b47b366043587f41507863df2f123617804b436ea2ff52d0/9.4.8/bin"
libdir     = "/workspaces/03-fold/.stack-work/install/x86_64-linux/f8297707d067d3a8b47b366043587f41507863df2f123617804b436ea2ff52d0/9.4.8/lib/x86_64-linux-ghc-9.4.8/hw3-fold-0.1.0.0-HzcQ0qroBfZEbeZr69xG4P-test"
dynlibdir  = "/workspaces/03-fold/.stack-work/install/x86_64-linux/f8297707d067d3a8b47b366043587f41507863df2f123617804b436ea2ff52d0/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/workspaces/03-fold/.stack-work/install/x86_64-linux/f8297707d067d3a8b47b366043587f41507863df2f123617804b436ea2ff52d0/9.4.8/share/x86_64-linux-ghc-9.4.8/hw3-fold-0.1.0.0"
libexecdir = "/workspaces/03-fold/.stack-work/install/x86_64-linux/f8297707d067d3a8b47b366043587f41507863df2f123617804b436ea2ff52d0/9.4.8/libexec/x86_64-linux-ghc-9.4.8/hw3-fold-0.1.0.0"
sysconfdir = "/workspaces/03-fold/.stack-work/install/x86_64-linux/f8297707d067d3a8b47b366043587f41507863df2f123617804b436ea2ff52d0/9.4.8/etc"

getBinDir     = catchIO (getEnv "hw3_fold_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hw3_fold_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hw3_fold_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hw3_fold_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw3_fold_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw3_fold_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
