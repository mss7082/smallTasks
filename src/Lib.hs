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

data GitStatus = UnstagedChanges | NotGitRepository | Clean | NoCommits deriving (Eq, Show)

data ProcessOutput = ProcessOutput
  { standardOut :: OutputBytes,
    errorOutput :: ErrorBytes
  }
  deriving (Eq, Show)

newtype OutputBytes = OutputBytes ByteString deriving (Eq, Show)

newtype ErrorBytes = ErrorBytes ByteString deriving (Eq, Show)

getProcessOutput :: String -> IO ProcessOutput
getProcessOutput commandString = do
  case words commandString of
    (command : arguments) -> do
      let processConfiguration = Process.setStderr Process.byteStringOutput $ Process.setStdout Process.byteStringOutput $ Process.proc command arguments
      Process.withProcessWait processConfiguration $ \process -> atomically $ do
        outBytes <- Process.getStdout process
        errorBytes <- Process.getStderr process
        let standardOut = OutputBytes $ LazyByteString.toStrict outBytes
            standardError = ErrorBytes $ LazyByteString.toStrict errorBytes
        pure $ ProcessOutput {standardOut = standardOut, errorOutput = standardError}
    [] -> error "Empty Command String"
