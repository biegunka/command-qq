{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- | Quasiquoters for external commands
module System.Command.QQ
  ( -- * Quasiquoters
    -- ** Default shell
    sh_, sh
    -- ** Constructors
  , shell, interpreter
    -- * Customizations
  , quoter, callCommand
  , Eval(..)
  , module System.Command.QQ.Embed
  ) where

import           Control.Applicative
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           System.Exit (ExitCode)
import           System.Posix.Env (getEnvDefault)
import qualified System.Process as P

import           System.Command.QQ.Embed

-- $setup
-- >>> :set -XQuasiQuotes


-- | Quasiquoter for the default shell
--
-- Constructs polymorphic action of type @Eval a => a@ from passed string.
--
-- Uses @SHELL@ environment variable as path to shell executable
-- or @\/bin\/sh@ if it is unset.
--
-- >>> [sh|echo "hello, world!"|] :: IO ExitCode
-- hello, world!
-- ExitSuccess
-- >>> [sh|echo "hello, world!"|] :: IO String
-- "hello, world!\n"
--
-- Haskell values can be embedded with Ruby-like syntax:
--
-- >>> let apples = 7
-- >>> [sh|echo "#{apples} apples!"|] :: IO String
-- "7 apples!\n"
sh :: QuasiQuoter
sh = quoter $ \string -> do
  shellEx <- runIO $ getEnvDefault "SHELL" "/bin/sh"
  callCommand shellEx ["-c"] string

-- | Simple quasiquoter for the default shell
--
-- 'sh' analog that always constructs an action of type
-- @IO ()@ and so can always be used without type annotations
--
-- >>> [sh_|echo "hello, world!"|]
-- hello, world!
sh_ :: QuasiQuoter
sh_ = quoter $ \string -> do
  shellEx <- runIO $ getEnvDefault "SHELL" "/bin/sh"
  callCommand_ shellEx ["-c"] string

-- | Shell's quasiquoter constructor
--
-- \"Shell\" here means executable that has the following API:
--
-- @
-- \<SHELL\> -c \<COMMAND\>
-- @
--
-- /e.g./ @sh@, @bash@, @zsh@, @ksh@, @tcsh@, @python@, etc
shell :: FilePath -> QuasiQuoter
shell path = quoter (callCommand path ["-c"])

-- | Interpreter's quasiquoter constructor
--
-- \"Interpreter\" here means executable that has the following API:
--
-- @
-- \<INTERPRETER\> -e \<COMMAND\>
-- @
--
-- /e.g./ @perl@, @ruby@, @ghc@, etc
interpreter :: FilePath -> QuasiQuoter
interpreter path = quoter (callCommand path ["-e"])


-- | Construct quasiquoter from function taking the string
-- and producing Haskell expression.
--
-- Other kinds of quasiquoters (patterns, types or
-- declarations quasiquoters) will fail in compile time
quoter :: (String -> Q Exp) -> QuasiQuoter
quoter quote = QuasiQuoter
  { quoteExp  = quote
  , quotePat  = failure "patterns"
  , quoteType = failure "types"
  , quoteDec  = failure "declarations"
  }
 where
  failure kind =
    fail $ "this quasiquoter does not support splicing " ++ kind

-- | Construct Haskell expression for external command call
callCommand
  :: FilePath -- ^ Command path
  -> [String] -- ^ Arguments that go to command before quasiquoter contents
  -> String   -- ^ Quasiquoter contents
  -> Q Exp
callCommand path args string =
  [e| eval path (args ++ [$(string2exp string)]) |]

-- | Construct Haskell expression for external command call
callCommand_
  :: FilePath -- ^ Command path
  -> [String] -- ^ Arguments that go to command before quasiquoter contents
  -> String   -- ^ Quasiquoter contents
  -> Q Exp
callCommand_ path args string =
  [e| eval path (args ++ [$(string2exp string)]) :: IO () |]

-- | Parse references to Haskell variables
string2exp :: String -> Q Exp
string2exp = raw where
  raw (break (== '#') -> parts) = case parts of
    (before, '#':'{' :after) -> [e| before ++ $(var after) |]
    (before, '#':'\\':after) -> [e| before ++ '#' : $(raw after) |]
    (before, '#':after)      -> [e| before ++ '#' : $(raw after) |]
    (before, [])             -> [e| before |]
    _                        -> fail "Should never happen"

  var (break (== '}') -> parts) = case parts of
     (before, '}':after) -> [e| embed $(return (VarE (mkName before))) ++ $(raw after) |]
     (before, _)         -> fail $ "Bad variable pattern: #{" ++ before


-- | Different interesting return types for quasiquoters
--
-- Instances here mostly resemble the types of things in "System.Process"
class Eval r where
  eval :: String -> [String] -> r

-- | Most basic instance: nothing is known about what happened in external command
--
-- >>> [sh|echo hello world|] :: IO ()
-- hello world
instance Eval (IO ()) where
  eval command args = () <$ P.rawSystem command args

-- | Return only exit code of external process
--
-- >>> [sh|echo hello world|] :: IO ExitCode
-- hello world
-- ExitSuccess
--
-- >>> [sh|exit 1|] :: IO ExitCode
-- ExitFailure 1
instance Eval (IO ExitCode) where
  eval = P.rawSystem

-- | Return only stdout of external process
--
-- Does not care if external process failed.
--
-- >>> [sh|echo hello world|] :: IO String
-- "hello world\n"
--
-- >>> [sh|echo hello world; return 1|] :: IO String
-- "hello world\n"
instance Eval (IO String) where
  eval command args = do
    (_, out, _) <- eval command args
    return out

-- | Return exit code, stdout, and stderr of external process
--
-- >>> [sh|echo hello world; echo bye world >&2; exit 1|] :: IO (ExitCode, String, String)
-- (ExitFailure 1,"hello world\n","bye world\n")
instance
  ( s ~ ExitCode
  , o ~ String
  , e ~ String
  ) => Eval (IO (s, o, e)) where
  eval command args = P.readProcessWithExitCode command args ""

-- | Return exit code, stdout, and stderr of external process
-- and consume stdin from supplied 'String'
--
-- >>> [sh|while read line; do echo ${#line}; done|] "hello\nworld!\n"
-- (ExitSuccess,"5\n6\n","")
instance
  ( i ~ String
  , o ~ (ExitCode, String, String)
  ) => Eval (i -> IO o) where
  eval = P.readProcessWithExitCode
