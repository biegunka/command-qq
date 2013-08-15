{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- | Quasiquoters for shell commands
module System.Shell.QQ
  ( sh, shell
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


-- | Quasiquoter for default shell
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
-- >>> let n = 7
-- >>> [sh|echo "#{n} apples!"|] :: IO String
-- "7 apples!\n"
--
-- Works only for expressions (obviously):
--
-- >>> return 3 :: IO [sh|blah|]
-- <BLANKLINE>
-- <interactive>:32:16:
--     Exception when trying to run compile-time code:
--       this quasiquoter does not support splicing types
--       Code: quoteType sh "blah"
sh :: QuasiQuoter
sh = expQuoter (quoteShellExp Nothing)

-- | Quasiquoters maker. For example, you can make quasiquoter for @\/bin\/bash@:
--
-- @
-- let bash = shell \"\/bin\/bash\"
-- [bash|echo hello|]
-- @
--
-- (Well, declaration and usage should be in different modules.)
--
-- Quasiquoter made that way never looks at @SHELL@ environment
-- variable but otherwise behaves exactly like 'sh'
shell :: FilePath -> QuasiQuoter
shell path = expQuoter (quoteShellExp (Just path))


-- | Different interesting return types for quasiquoters
--
-- Instances mostly resemble the types of things in "System.Process"
--
-- 'eval' is not supposed to be used directly, use quasiquoters instead
class Eval r where
  eval :: String -> [String] -> r

-- | Most basic instance: nothing is known about what happened in shell
--
-- >>> [sh|echo hello world|] :: IO ()
-- hello world
instance Eval (IO ()) where
  eval command args = () <$ P.rawSystem command args

-- | Return only exit code of shell process
--
-- >>> [sh|echo hello world|] :: IO ExitCode
-- hello world
-- ExitSuccess
--
-- >>> [sh|exit 1|] :: IO ExitCode
-- ExitFailure 1
instance Eval (IO ExitCode) where
  eval command args = P.rawSystem command args

-- | Return only stdout of shell process
--
-- Does not care if shell process failed.
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

-- | Return exit code, stdout, and stderr of shell process
--
-- >>> [sh|echo hello world; echo bye world >&2; exit 1|] :: IO (ExitCode, String, String)
-- (ExitFailure 1,"hello world\n","bye world\n")
instance
  ( s ~ ExitCode
  , o ~ String
  , e ~ String
  ) => Eval (IO (s, o, e)) where
  eval command args = P.readProcessWithExitCode command args ""

-- | Return exit code, stdout, and stderr of shell process
-- and consume stdin from supplied 'String'
--
-- >>> [sh|while read line; do echo ${#line}; done|] "hello\nworld!\n"
-- (ExitSuccess,"5\n6\n","")
instance
  ( i  ~ String
  , o ~ IO (ExitCode, String, String)
  ) => Eval (i -> o) where
  eval command args stdin = P.readProcessWithExitCode command args stdin


-- | Embed haskell values into shell scripts
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


-- | Generic quasiquoter for shell calls
expQuoter :: (String -> Q Exp) -> QuasiQuoter
expQuoter quote = QuasiQuoter
  { quoteExp  = quote
  , quotePat  = failure "patterns"
  , quoteType = failure "types"
  , quoteDec  = failure "declarations"
  }
 where
  failure kind =
    fail $ "this quasiquoter does not support splicing " ++ kind

-- | Construct shell call
quoteShellExp :: Maybe FilePath -> String -> Q Exp
quoteShellExp path s = do
  shellEx <- runIO $ maybe (getEnvDefault "SHELL" "/bin/sh") return path
  [e| eval shellEx ["-c", $(string2exp s)] |]

-- | Parse references to Haskell variables
string2exp :: String -> Q Exp
string2exp = raw where
  raw (break (== '#') -> parts) = case parts of
    (before, '#':'{':after) -> [e| before ++ $(var after)|]
    (before, '#':after)     -> [e| before ++ '#' : $(raw after)|]
    (before, [])            -> [e| before |]
    _ -> fail $ "Should never happen"

  var (break (== '}') -> parts) = case parts of
     (before, '}':after) -> [e| embed $(return (VarE (mkName before))) ++ $(raw after) |]
     (before, _)         -> fail $ "Bad variable pattern: #{" ++ before
