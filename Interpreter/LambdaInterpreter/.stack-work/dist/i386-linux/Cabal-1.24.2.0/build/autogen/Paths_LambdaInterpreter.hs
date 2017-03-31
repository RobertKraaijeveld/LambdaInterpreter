{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_LambdaInterpreter (
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

bindir     = "/home/robert/Documents/Projects/Haskell/HaskellLambdaInterpreter/Haskell/LambdaInterpreter/.stack-work/install/i386-linux/lts-8.6/8.0.2/bin"
libdir     = "/home/robert/Documents/Projects/Haskell/HaskellLambdaInterpreter/Haskell/LambdaInterpreter/.stack-work/install/i386-linux/lts-8.6/8.0.2/lib/i386-linux-ghc-8.0.2/LambdaInterpreter-0.1.0.0-JHYgXxxUj6lFqpIq8bZ5HD"
dynlibdir  = "/home/robert/Documents/Projects/Haskell/HaskellLambdaInterpreter/Haskell/LambdaInterpreter/.stack-work/install/i386-linux/lts-8.6/8.0.2/lib/i386-linux-ghc-8.0.2"
datadir    = "/home/robert/Documents/Projects/Haskell/HaskellLambdaInterpreter/Haskell/LambdaInterpreter/.stack-work/install/i386-linux/lts-8.6/8.0.2/share/i386-linux-ghc-8.0.2/LambdaInterpreter-0.1.0.0"
libexecdir = "/home/robert/Documents/Projects/Haskell/HaskellLambdaInterpreter/Haskell/LambdaInterpreter/.stack-work/install/i386-linux/lts-8.6/8.0.2/libexec"
sysconfdir = "/home/robert/Documents/Projects/Haskell/HaskellLambdaInterpreter/Haskell/LambdaInterpreter/.stack-work/install/i386-linux/lts-8.6/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "LambdaInterpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "LambdaInterpreter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "LambdaInterpreter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "LambdaInterpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "LambdaInterpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "LambdaInterpreter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
