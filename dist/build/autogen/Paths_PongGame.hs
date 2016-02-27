module Paths_PongGame (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/rory/Documents/PongGame/.cabal-sandbox/bin"
libdir     = "/home/rory/Documents/PongGame/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/PongGame-0.1.0.0"
datadir    = "/home/rory/Documents/PongGame/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/PongGame-0.1.0.0"
libexecdir = "/home/rory/Documents/PongGame/.cabal-sandbox/libexec"
sysconfdir = "/home/rory/Documents/PongGame/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PongGame_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PongGame_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "PongGame_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PongGame_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PongGame_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
