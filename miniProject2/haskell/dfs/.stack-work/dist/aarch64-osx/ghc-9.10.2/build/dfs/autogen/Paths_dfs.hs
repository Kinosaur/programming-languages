{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_dfs (
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
bindir     = "/Users/kaungkhantlin/Developer/1_2025/programming-languages/miniProject2/haskell/dfs/.stack-work/install/aarch64-osx/c78f44e280091ae3ad87365000415eeba04bd5428fcc403b489b919ce70727d6/9.10.2/bin"
libdir     = "/Users/kaungkhantlin/Developer/1_2025/programming-languages/miniProject2/haskell/dfs/.stack-work/install/aarch64-osx/c78f44e280091ae3ad87365000415eeba04bd5428fcc403b489b919ce70727d6/9.10.2/lib/aarch64-osx-ghc-9.10.2-b8ed/dfs-0.1.0.0-71TxvwNupAR6mTd0Xi6qja-dfs"
dynlibdir  = "/Users/kaungkhantlin/Developer/1_2025/programming-languages/miniProject2/haskell/dfs/.stack-work/install/aarch64-osx/c78f44e280091ae3ad87365000415eeba04bd5428fcc403b489b919ce70727d6/9.10.2/lib/aarch64-osx-ghc-9.10.2-b8ed"
datadir    = "/Users/kaungkhantlin/Developer/1_2025/programming-languages/miniProject2/haskell/dfs/.stack-work/install/aarch64-osx/c78f44e280091ae3ad87365000415eeba04bd5428fcc403b489b919ce70727d6/9.10.2/share/aarch64-osx-ghc-9.10.2-b8ed/dfs-0.1.0.0"
libexecdir = "/Users/kaungkhantlin/Developer/1_2025/programming-languages/miniProject2/haskell/dfs/.stack-work/install/aarch64-osx/c78f44e280091ae3ad87365000415eeba04bd5428fcc403b489b919ce70727d6/9.10.2/libexec/aarch64-osx-ghc-9.10.2-b8ed/dfs-0.1.0.0"
sysconfdir = "/Users/kaungkhantlin/Developer/1_2025/programming-languages/miniProject2/haskell/dfs/.stack-work/install/aarch64-osx/c78f44e280091ae3ad87365000415eeba04bd5428fcc403b489b919ce70727d6/9.10.2/etc"

getBinDir     = catchIO (getEnv "dfs_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "dfs_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "dfs_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "dfs_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "dfs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "dfs_sysconfdir") (\_ -> return sysconfdir)



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
