module Paths_monad_memo (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/eduardsergeev/.cabal/bin"
libdir     = "/Users/eduardsergeev/.cabal/lib/monad-memo-0.1.0/ghc-6.12.3"
datadir    = "/Users/eduardsergeev/.cabal/share/monad-memo-0.1.0"
libexecdir = "/Users/eduardsergeev/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "monad_memo_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "monad_memo_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "monad_memo_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "monad_memo_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
