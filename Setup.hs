import Distribution.Simple
import Distribution.Simple.InstallDirs (InstallDirs(..), fromPathTemplate, toPathTemplate)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup (ConfigFlags(..), fromFlag, fromFlagOrDefault)
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)

main = defaultMainWithHooks simpleUserHooks {postInst = postInstall}

postInstall _ _ _ buildInfo = do
  -- I tried the following seemingly more correct way but received the error:
  -- "internal error InstallDirs.libsubdir". For now I gave up on tracking it
  -- down and switched to the following code. If you know what I'm doing wrong
  -- here, please let me know.
  -- let installDirs = absoluteInstallDirs pkgDesc buildInfo NoCopyDest
  let dirs = configInstallDirs $ configFlags buildInfo
      dir = (fromPathTemplate . fromFlag $ prefix dirs) </> (fromPathTemplate . fromFlagOrDefault (toPathTemplate "bin") $ bindir dirs)
  createSymbolicLink (dir </> "redo") (dir </> "redo-ifchange")
