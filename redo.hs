{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK prune #-}

module Main where

import Control.Exception (catch, catchJust, IOException)
import Control.Monad (filterM, liftM, unless, guard)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import Data.Map.Lazy (insert, fromList, toList, adjust)
import Data.Maybe (listToMaybe)
-- import Debug.Trace (traceShow)
import GHC.IO.Exception (IOErrorType(..))
import System.Directory (renameFile, removeFile, doesFileExist, getDirectoryContents, removeDirectoryRecursive, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.Environment (getArgs, getEnvironment, getProgName, lookupEnv)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (hasExtension, replaceBaseName, takeBaseName, (</>), splitFileName)
import System.IO (hPutStrLn, stderr, hGetLine, withFile, IOMode(..), hFileSize)
import System.IO.Error (ioeGetErrorType, isDoesNotExistError)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..))

-- traceShow' arg = traceShow arg arg

-- | This is the directory that redo will store and read metadata on targets from.
metaDir = ".redo"

main :: IO ()
main = do
  -- The redo function depends on the given target existing in the current
  -- working directory. We keep track of the directory we were invoked from
  -- here, changing the current working directory as necessary for each target
  -- specified on the commandline.
  topDir <- getCurrentDirectory
  getArgs >>= mapM_ (\arg -> do
    -- Separate out the directory component from the target. If there was no
    -- directory specified, dir will we "./".
    let (dir, file) = splitFileName arg
    setCurrentDirectory dir
    -- redo doesn't make use of dir other than to use it to output a "progress"
    -- line if it decides that it needs to rebuild the target. I'd like to get
    -- rid of this argument, but it will require non-trivial refactoring.
    redo file dir
    setCurrentDirectory topDir)
  progName <- getProgName
  -- The `REDO_TARGET` environment variable is set when `redo` or
  -- `redo-ifchange` is spawned by another `redo` process. This lets us know
  -- that we're in a recursive context and its value tells us the name of the
  -- parent target. TODO: Maybe this should be renamed to REDO_PARENT_TARGET?
  redoTarget' <- lookupEnv "REDO_TARGET"
  -- We look at the name that we were invoked as to determine how to behave.
  -- Doing so allows us to share functionality between `redo` and
  -- `redo-ifchange` without building 2 separate programs: instead we can just
  -- symlink `redo-ifchange` to `redo` (or make a copy if symlinks aren't
  -- available on the filesystem).
  case (progName, redoTarget') of
    -- The only thing special about `redo-ifchange` is that it writes out MD5
    -- information for the dependencies it was provided with. For example, a
    -- .do script like:
    --     redo-ifchange source.hs
    -- tells later invocations of redo to check if source.hs has changed, whereas
    --     redo source.hs
    -- effectively says to rebuild source.hs if necessary but don't note it as
    -- a dependency of this target. Thinking about this, I can't really see a
    -- useful case for doing so, so later versions of redo might instead change
    -- the meaning to be "force the (re)building of this dependency".
    ("redo-ifchange", Just redoTarget) -> mapM_ (writeMD5 redoTarget) =<< getArgs
    ("redo-ifchange", Nothing) -> error "Missing REDO_TARGET environment variable."
    -- There's no case here for "redo" because it doesn't do any bookkeeping.
    _ -> return ()

-- | Rebuild a given target if it's out-of-date or doesn't exist.
redo :: String    -- ^ target (file) name
     -> FilePath  -- ^ the current directory (for output purposes only)
     -> IO ()
redo target dir = do
  upToDate' <- upToDate target
  unless upToDate' $ maybe missingDo redo' =<< doPath target
 where redo' :: FilePath -> IO ()
       redo' path = do
         -- At this point, we know that the target is out-of-date so we're just
         -- going to rebuild it. We send a progress line to stderr to let the
         -- user know that we're now (re)building this target. This should be
         -- the only place that the `dir` argument is used.
         hPutStrLn stderr $ "redo " ++ (if dir == "./" then "" else dir) ++ target
         -- Remove the contents of the `.redo/target` metadata directory if it
         -- exists because it's possibly stale (we're going to get a fresh list
         -- of dependencies when we run the .do script).
         catchJust (guard . isDoesNotExistError)
                   (removeDirectoryRecursive metaDepsDir)
                   (\_ -> return ())
         createDirectoryIfMissing True metaDepsDir
         -- A redo target implicitly depends on the .do script for the target
         -- itself, so we note that here by writing out the MD5 for the .do
         -- script.
         writeMD5 target path
         -- Read the existing environment variable list so that we can
         -- append/overwrite the REDO_TARGET variable with the value of the
         -- current target. Any `redo-ifchange` call in the .do script depends
         -- on this information.
         oldEnv <- getEnvironment
         -- TODO: Remove `.` from the PATH now that we have an install target.
         let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
         (_, _, _, ph) <- createProcess $ (shell $ cmd path) {env = Just newEnv}
         -- Wait for this .do script (and any recursive invocations of `redo`
         -- or `redo-ifchange`) to complete before continuing.
         exit <- waitForProcess ph
         case exit of
           ExitSuccess -> do
             -- OK, the target (temporary) file is presumed to have been built
             -- correctly. If the file is 0 bytes, the .do script didn't
             -- produce any output. Since there was no error indicated by the
             -- build script, we consider this a "phony" or empty target. An
             -- example of an empty target is "install.do": it takes an action
             -- somewhere but doesn't produce a file in the current directory.
             size <- fileSize tmp
             if size > 0
                    -- Valid output produced, atomically rename it in place.
               then renameFile tmp target
               else removeFile tmp
           -- If the .do script returns a non-zero error code for any reason,
           -- we consider the build to have failed, output a message on stderr,
           -- and remove the temporary file. This means that the target file
           -- shouldn't be touched.
           ExitFailure code -> do hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
                                  removeFile tmp
                                  exitWith $ ExitFailure code
       -- The temporary file name is simply target---redoing, arbitrarily
       -- chosen to match the implementation of redo in Alan Grosskurth's
       -- thesis paper.
       -- TODO: As suggested by viewer Rotten194, we probably want to switch to
       -- using a file in the temporary file system, taking advantage of any
       -- faster/restricted temporary file system (RAM, partition, etc.).
       tmp = target ++ "---redoing"
       -- Every target in the current directory records dependencies in the
       -- directory .redo/target
       metaDepsDir = metaDir </> target
       missingDo = do
         exists <- doesFileExist target
         unless exists $ error $ "No .do file found for target '" ++ target ++ "'"
       -- All .do scripts are invoked by sh. We use the -e argument to cause
       -- the script to bail on any errors. The first argument is 0 to indicate
       -- that it's unused (but to maintain the ordering of the other arguments
       -- for compatibility with other redo implementations). The second
       -- argument is just the basename of the target (the file name with the
       -- last (if any) extension removed).
       -- Note that the temporary file is provided as a third argument to the
       -- script while stdout is also directed to it. TODO: This could lead to
       -- some confusion about what happens if both are used (output is both
       -- sent to stdout and written to $3), and the behavior should at least
       -- be documented.
       cmd path = unwords ["sh -e", path, "0", takeBaseName target, tmp, ">", tmp]

-- | Determine the .do script path for a given target name.
doPath :: FilePath             -- ^ the target (file) name
       -> IO (Maybe FilePath)  -- ^ the preferred .do script file if one exists
doPath target = listToMaybe `liftM` filterM doesFileExist candidates
 -- There are only 2 potential candidate .do scripts: an exact match and an
 -- extension-based default.
 where candidates = (target ++ ".do") : if hasExtension target
                                        then [replaceBaseName target "default" ++ ".do"]
                                        else []

-- | Determine whether or not a target is up-to-date (i.e. it exists and none of its dependencies have changed or are out-of-date themselves).
upToDate :: FilePath  -- ^ the target (file) name
         -> IO Bool   -- ^ True if the target is up-to-date
upToDate target = catch
  (do exists <- doesFileExist target
      if exists
        -- If the target exists, it's up-to-date it all of its dependencies are
        -- up-to-date.
        then do md5s <- getDirectoryContents (metaDir </> target)
                and `liftM` mapM depUpToDate md5s
        -- If the target doesn't exist, it's certainly out-of-date.
        else return False)
  (\(_ :: IOException) -> return False)
 where depUpToDate :: String -> IO Bool
       depUpToDate oldMD5 = catch
             -- To avoid having to escape directory path separators (such as
             -- converting / to !), each dependency is stored in a file named
             -- by its MD5. e.g. a dependency named `redo.hs` with an MD5 of
             -- 29e57f39b7ea2795ab2e452ada562778 and for the target `redo`
             -- would be stored in the file
             -- `.redo/redo/29e57f39b7ea2795ab2e452ada562778`. The contents of
             -- the file would be the relative path to the dependency (in this
             -- case, simply "redo.hs").
         (do dep <- withFile (metaDir </> target </> oldMD5) ReadMode hGetLine
             newMD5 <- fileMD5 dep
             doScript <- doPath dep
             case doScript of
               -- Simple dependencies are up-to-date if their MD5s match that
               -- of the last time that they were checked.
               Nothing -> return $ oldMD5 == newMD5
               -- But a dependency can itself depend on other files, so aside
               -- from just having their MD5s match between runs, all of their
               -- dependencies must also be up-to-date.
               Just _ -> do upToDate' <- upToDate dep
                            return $ (oldMD5 == newMD5) && upToDate')
         -- If the target metadata directory (e.g. `.redo/redo`) doesn't exist
         -- (for example, on the first run of redo), we trap the IO exception
         -- and just indicate that the target is out-of-date.
         -- TODO: The way this is written, it looks as if it would say that the
         -- target /is/ up-to-date if the metadata directory is missing, which
         -- would be wrong.
         (\e -> return (ioeGetErrorType e == InappropriateType))

-- | Calculate the MD5 checksum of a file.
--
-- For example:
--
-- >>> fileMD5 "redo.hs"
-- "29e57f39b7ea2795ab2e452ada562778"
fileMD5 :: FilePath   -- ^ the file to calculate the checksum of
        -> IO String  -- ^ a 32-character MD5 checksum
fileMD5 path = (show . MD5.md5) `liftM` BL.readFile path

-- TODO: This function is confusing (which are we writing which for?).
-- | Write out the MD5 checksum of a given dependency of a target.
writeMD5 :: String    -- ^ the target (file) for this dependency
         -> FilePath  -- ^ the file path of the dependency
         -> IO ()
writeMD5 redoTarget dep = do
  md5 <- fileMD5 dep
  writeFile (metaDir </> redoTarget </> md5) dep

-- | Determine a file's size.
fileSize :: FilePath    -- ^ the file to determine the size of
         -> IO Integer  -- ^ the file's size in bytes
fileSize path = withFile path ReadMode hFileSize
-- I don't know of a more efficient way to determine file size in a
-- cross-platform way.
