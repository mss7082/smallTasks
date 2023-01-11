{-# LANGUAGE MultiWayIf #-}

module Lib
  ( runMain,
  )
where

import RIO
import RIO.ByteString (ByteString)
import RIO.ByteString qualified as ByteString
import RIO.ByteString.Lazy qualified as LazyByteString
import System.IO (putStrLn)
import System.Process.Typed qualified as Process

runMain :: IO ()
runMain = putStrLn "someFunc"

data GitStatus
  = UnstagedChanges
  | NotGitRepository
  | Clean
  | NoCommits
  | UnknownStatus ProcessOutput
  deriving (Eq, Show)

data ProcessOutput = ProcessOutput
  { standardOut :: OutputBytes,
    errorOutput :: ErrorBytes
  }
  deriving (Eq, Show)

newtype OutputBytes = OutputBytes ByteString deriving (Eq, Show)

newtype WorkingDirectory = WorkingDirectory FilePath deriving (Eq, Show)

newtype CommandString = CommandString String deriving (Eq, Show)

newtype ErrorBytes = ErrorBytes ByteString deriving (Eq, Show)

getGitStatus :: FilePath -> IO GitStatus
getGitStatus path = do
  processOutput@ProcessOutput
    { standardOut = OutputBytes outBytes,
      errorOutput = ErrorBytes errorBytes
    } <-
    getProcessOutput
      (WorkingDirectory path)
      (CommandString "git status")
  if
      | "Changes not staged for commit" `ByteString.isInfixOf` outBytes -> pure UnstagedChanges
      | "No commits yet" `ByteString.isInfixOf` outBytes -> pure NoCommits
      | "fatal: not a git repository (or any of the parent directories): .git" `ByteString.isInfixOf` errorBytes -> pure NotGitRepository
      | "nothing to commit, working tree clean" `ByteString.isInfixOf` outBytes -> pure Clean
      | otherwise -> pure $ UnknownStatus processOutput

getProcessOutput :: WorkingDirectory -> CommandString -> IO ProcessOutput
getProcessOutput (WorkingDirectory workingDirectory) (CommandString commandString) = do
  case words commandString of
    (command : arguments) -> do
      let processConfiguration = Process.setWorkingDir workingDirectory $ Process.setStderr Process.byteStringOutput $ Process.setStdout Process.byteStringOutput $ Process.proc command arguments
      Process.withProcessWait processConfiguration $ \process -> atomically $ do
        outBytes <- Process.getStdout process
        errorBytes <- Process.getStderr process
        let standardOut = OutputBytes $ LazyByteString.toStrict outBytes
            standardError = ErrorBytes $ LazyByteString.toStrict errorBytes
        pure $ ProcessOutput {standardOut = standardOut, errorOutput = standardError}
    [] -> error "Empty Command String"
