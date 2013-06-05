{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch, IOException)
import Control.Monad (filterM, liftM, unless)
import Data.Map.Lazy (insert, fromList, toList, adjust)
import Data.Maybe (listToMaybe)
import Data.Typeable (typeOf)
import Debug.Trace (traceShow)
import GHC.IO.Exception (IOErrorType(..))
import System.Directory (renameFile, removeFile, doesFileExist, getDirectoryContents)
import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName, takeBaseName, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorType)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..), StdStream(..), CmdSpec(..))

traceShow' arg = traceShow arg arg

main :: IO ()
main = mapM_ redo =<< getArgs

redo :: String -> IO ()
redo target = do
  upToDate' <- upToDate target
  unless upToDate' $ maybe printMissing redo' =<< redoPath target
 where redo' :: FilePath -> IO ()
       redo' path = do
         oldEnv <- getEnvironment
         let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
         (_, _, _, ph) <- createProcess $ (shell $ cmd path) {env = Just newEnv}
         exit <- waitForProcess ph
         case exit of
           ExitSuccess -> do renameFile tmp target
           ExitFailure code -> do hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
                                  removeFile tmp
       tmp = target ++ "---redoing"
       printMissing = error $ "No .do file found for target '" ++ target ++ "'"
       cmd path = traceShow' $ unwords ["sh", path, "0", takeBaseName target, tmp, ">", tmp]

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = listToMaybe `liftM` filterM doesFileExist candidates
 where candidates = [target ++ ".do"] ++ if hasExtension target
                                         then [replaceBaseName target "default" ++ ".do"]
                                         else []

upToDate :: String -> IO Bool
upToDate target = catch
  (do deps <- getDirectoryContents depDir
      (traceShow' . all id) `liftM` mapM depUpToDate deps)
  (\(e :: IOException) -> return False)
 where depDir = ".redo" </> target
       depUpToDate :: FilePath -> IO Bool
       depUpToDate dep = catch
         (do oldMD5 <- traceShow' `liftM` readFile (depDir </> dep)
             return False)
         (\e -> return (ioeGetErrorType e == InappropriateType))
