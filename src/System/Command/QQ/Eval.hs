{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Evalute passed arguments with external interpreter
module System.Command.QQ.Eval
  ( Eval(..)
  ) where

import           Control.Concurrent
import           Control.Exception (evaluate, mask, onException)
import           Control.Monad
import           Data.Foldable (traverse_)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import           System.Exit (ExitCode)
import qualified System.Process as P
import           System.IO (hFlush, hClose)

-- $setup
-- >>> import System.Command.QQ


-- | Different interesting return types for quasiquoters
--
-- Instances here mostly resemble the types of things in "System.Process"
class Eval r where
  eval :: String -> [String] -> r

-- | The most basic instance: nothing is known about what happened in external command
--
-- External command's stdout and stderr go to caller's stdout and stderr respectively
--
-- >>> [sh|echo hello world|] :: IO ()
-- hello world
instance Eval (IO ()) where
  eval command = void . P.rawSystem command

-- | Return exit code of the external process
--
-- >>> [sh|exit 0|] :: IO ExitCode
-- ExitSuccess
--
-- >>> [sh|exit 7|] :: IO ExitCode
-- ExitFailure 7
instance Eval (IO ExitCode) where
  eval command args = do
    (s, _, _) <- eval command args Text.empty
    return s

-- | Return stdout of the external process as 'Text'
--
-- Does not care whether external process has failed or not.
--
-- >>> [sh|echo -n hello world|] :: IO Text
-- "hello world"
instance Eval (IO Text) where
  eval command args = do
    (_, o, _) <- eval command args
    return o

-- | Return stdout of external process as 'String'
--
-- Does not care whether external process has failed or not.
--
-- >>> [sh|echo -n hello world|] :: IO String
-- "hello world"
instance Eval (IO String) where
  eval command = fmap Text.unpack . eval command

-- | Return exit code, stdout, and stderr of external process
--
-- >>> [sh|echo hello world; echo bye world >&2; exit 1|] :: IO (ExitCode, Text, Text)
-- (ExitFailure 1,"hello world\n","bye world\n")
instance
  ( s ~ ExitCode
  , o ~ Text
  , e ~ Text
  ) => Eval (IO (s, o, e)) where
  eval command args = eval command args Text.empty

-- | Return exit code, stdout, and stderr of the external process
-- and pass supplied 'Text' to its stdin
--
-- >>> [sh|while read line; do echo ${#line}; done|] "hello\nworld!\n"
-- (ExitSuccess,"5\n6\n","")
instance
  ( i ~ Text
  , o ~ (ExitCode, Text, Text)
  ) => Eval (i -> IO o) where
  eval = readProcessWithExitCode

readProcessWithExitCode :: String -> [String] -> Text -> IO (ExitCode, Text, Text)
readProcessWithExitCode cmd args input =
  mask $ \restore -> do
    (Just inh, Just outh, Just errh, pid) <-
        P.createProcess (P.proc cmd args)
          { P.std_in  = P.CreatePipe
          , P.std_out = P.CreatePipe
          , P.std_err = P.CreatePipe
          }

    onException
      (restore $ do
        var <- newEmptyMVar
        out <- Text.hGetContents outh
        err <- Text.hGetContents errh

        forkFinally (evaluate (Text.length out)) (\_ -> putMVar var ())
        forkFinally (evaluate (Text.length err)) (\_ -> putMVar var ())

        unless (Text.null input) $
          Text.hPutStr inh input >> hFlush inh
        hClose inh

        takeMVar var
        takeMVar var
        hClose outh
        hClose errh

        s <- P.waitForProcess pid

        return (s, out, err))
      (do P.terminateProcess pid
          traverse_ hClose [inh, outh, errh]
          P.waitForProcess pid)
