{-# LANGUAGE CPP #-}

import Control.Monad (unless)
import Distribution.Simple
import Distribution.Simple.InstallDirs (InstallDirs(..), fromPathTemplate, toPathTemplate)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup (ConfigFlags(..), fromFlag, fromFlagOrDefault)
import System.Directory (copyFile, doesFileExist)
import System.FilePath ((</>))
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#else
import System.Posix.Files (createSymbolicLink)
#endif

main = defaultMainWithHooks simpleUserHooks {postInst = postInstall}

postInstall _ _ _ buildInfo = do
  -- I tried the following seemingly more correct way but received the error:
  -- "internal error InstallDirs.libsubdir". For now I gave up on tracking it
  -- down and switched to the following code. If you know what I'm doing wrong
  -- here, please let me know.
  -- let installDirs = absoluteInstallDirs pkgDesc buildInfo NoCopyDest
  let dirs = configInstallDirs $ configFlags buildInfo
      dir = (fromPathTemplate . fromFlag $ prefix dirs) </> (fromPathTemplate . fromFlagOrDefault (toPathTemplate "bin") $ bindir dirs)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  copyFile (dir </> "redo.exe") (dir </> "redo-ifchange.exe")
#else
  let symlink = (dir </> "redo-ifchange")
  exists <- doesFileExist symlink
  unless exists $ createSymbolicLink (dir </> "redo") symlink
#endif
