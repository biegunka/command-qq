{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- | Quasiquoters for external commands
module System.Command.QQ
  ( -- * Quasiquoters
    sh, shell, interpreter
    -- * Customizations
  , quoter, callCommand
  , Eval(..), Embed(..)
  ) where

import           Control.Applicative ((<$), pure)
import           Data.Int
import           Data.Word
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           System.Exit (ExitCode)
import           System.Posix.Env (getEnvDefault)
import qualified System.Process as P

-- $setup
-- >>> :set -XQuasiQuotes


-- | Quasiquoter for the default shell
--
-- \"default\" here means it uses value of @SHELL@ environment variable
-- or @\/bin\/sh@ if it is not set.
--
-- >>> [sh|echo "hi!"|] :: IO ExitCode
-- hi!
-- ExitSuccess
-- >>> [sh|echo "hi!"|] :: IO String
-- "hi!\n"
--
-- Haskell values can be embedded with Ruby-like syntax:
--
-- >>> let apples = 7
-- >>> [sh|echo "#{apples} apples!"|] :: IO String
-- "7 apples!\n"
--
-- Works only for expressions (obviously):
--
-- >>> return 3 :: IO [sh|blah|]
-- <BLANKLINE>
-- <interactive>:116:16:
--     Exception when trying to run compile-time code:
--       this quasiquoter does not support splicing types
--       Code: quoteType sh "blah"
sh :: QuasiQuoter
sh = quoter $ \string -> do
  shellEx <- runIO $ getEnvDefault "SHELL" "/bin/sh"
  callCommand shellEx ["-c"] string

-- | Shell commands quasiquoter maker
--
-- \"Shell\" here means something that implements the following interface:
--
-- @
-- \<SHELL\> -c \<COMMAND\>
-- @
--
-- /e.g./ @sh@, @bash@, @zsh@, @ksh@, @tcsh@, @python@, etc
--
-- Everything that applies to 'sh' applies to 'shell'
shell :: FilePath -> QuasiQuoter
shell path = quoter (callCommand path ["-c"])

-- | Interpreter commands quasiquoter maker
--
-- \"Interpreter\" here means something that implements the following interface:
--
-- @
-- \<INTERPRETER\> -e \<COMMAND\>
-- @
--
-- /e.g./ @perl@, @ruby@, @ghc@, etc
--
-- Everything that applies to 'sh' applies to 'interpreter'
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

-- | Parse references to Haskell variables
string2exp :: String -> Q Exp
string2exp = raw where
  raw (break (== '#') -> parts) = case parts of
    (before, '#':'{' :after) -> [e| before ++ $(var after) |]
    (before, '#':'\\':after) -> [e| before ++ '#' : $(raw after) |]
    (before, '#':after)      -> [e| before ++ '#' : $(raw after) |]
    (before, [])             -> [e| before |]
    _ -> fail $ "Should never happen"

  var (break (== '}') -> parts) = case parts of
     (before, '}':after) -> [e| embed $(return (VarE (mkName before))) ++ $(raw after) |]
     (before, _)         -> fail $ "Bad variable pattern: #{" ++ before


-- | Different interesting return types for quasiquoters
--
-- Instances here mostly resemble the types of things in "System.Process"
--
-- "System.Command.QQ.CommandT" shows how to use 'Eval' to provide nice DSL
-- for sequencing external commands
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
  eval command args = P.rawSystem command args

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
  eval command args stdin = P.readProcessWithExitCode command args stdin


-- | Embed haskell values into external commands
--
-- I recommend using @-XExtendedDefaultRules@ for modules
-- where you want to embed values, it would save for annoying
-- type annotations for numeric literals
--
-- @
-- embed . embed = embed
-- @
class Embed a where
  embed :: a -> String
  default embed :: Show a => a -> String
  embed = show

-- |
-- >>> embed 4
-- "4"
--
-- >>> embed (7 :: Integer)
-- "7"
instance Embed Integer

-- |
-- >>> embed (7 :: Int)
-- "7"
instance Embed Int
instance Embed Int8
instance Embed Int16
instance Embed Int32
instance Embed Int64
instance Embed Word
instance Embed Word8
instance Embed Word16
instance Embed Word32
instance Embed Word64

-- |
-- >>> embed (7 :: Float)
-- "7.0"
instance Embed Float

-- |
-- >>> embed 4.0
-- "4.0"
--
-- >>> embed (7 :: Double)
-- "7.0"
instance Embed Double

-- |
-- >>> embed 'c'
-- "c"
instance Embed Char where
  embed = pure

-- |
-- >>> embed "hi"
-- "hi"
instance Embed String where
  embed = id
