{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | QuasiQuoters
module System.Shell.QQ
  ( sh
  , Eval(..), Embed(..)
  ) where

import           Control.Applicative ((<$), pure)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           System.Exit (ExitCode)
import           System.Posix.Env (getEnvDefault)
import qualified System.Process as Proc

-- $setup
-- >>> :set -XQuasiQuotes


-- | QuasiQuoter for shell scripts
--
-- Works only for expressions (obviously)
--
-- >>> return 3 :: IO [sh|blah|]
-- <BLANKLINE>
-- <interactive>:24:16:
--     Exception when trying to run compile-time code:
--       sh quasiquoter does not support splicing types
--       Code: quoteType sh "blah"
sh :: QuasiQuoter
sh = QuasiQuoter
  { quoteExp  = quoteShellExp
  , quotePat  = failure "patterns"
  , quoteType = failure "types"
  , quoteDec  = failure "declarations"
  }
 where
  failure kind =
    fail $ "sh quasiquoter does not support splicing " ++ kind


-- | Different interesting return types for 'sh' QuasiQuoter
--
-- Instances mostly resemble the types of things in "System.Process"
--
-- 'eval' is not supposed to be used directly, user is expected to use
-- 'sh' quasiquoter instead
class Eval r where
  eval :: String -> [String] -> r

-- | Most basic instance: nothing is known about what happened in shell
--
-- >>> [sh|echo hello world|] :: IO ()
-- hello world
instance Eval (IO ()) where
  eval command args = () <$ Proc.rawSystem command args

-- | Return only exit code of shell process
--
-- >>> [sh|echo hello world|] :: IO ExitCode
-- hello world
-- ExitSuccess
--
-- >>> [sh|return 1|] :: IO ExitCode
-- ExitFailure 1
instance Eval (IO ExitCode) where
  eval command args = Proc.rawSystem command args

-- | Return only stdout of shell process
--
-- >>> [sh|echo hello world|] :: IO String
-- "hello world\n"
instance Eval (IO String) where
  eval command args = Proc.readProcess command args ""

-- | Return exit code, stdout, and stderr of shell process
--
-- >>> [sh|echo hello world; echo bye world >&2; return 1|] :: IO (ExitCode, String, String)
-- (ExitFailure 1,"hello world\n","bye world\n")
instance
  ( status ~ ExitCode
  , out    ~ String
  , err    ~ String
  ) => Eval (IO (status, out, err)) where
  eval command args = Proc.readProcessWithExitCode command args ""

-- | Return exit code, stdout, and stderr of shell process
-- and consume stdin from supplied 'String'
--
-- >>> [sh|while read line; do echo \${#line}; done|] "hello\nworld\n"
-- (ExitSuccess,"5\n5\n","")
instance
  ( input  ~ String
  , output ~ IO (ExitCode, String, String)
  ) => Eval (input -> output) where
  eval command args stdin = Proc.readProcessWithExitCode command args stdin


-- | Embed haskell values into shell scripts
--
-- Instances provided for all "Prelude" data types for
-- which it makes sense
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

-- |
-- >>> embed 4
-- "4"
--
-- >>> embed (7 :: Integer)
-- "7"
instance Embed Integer where
  embed = show

-- |
-- >>> embed (7 :: Int)
-- "7"
instance Embed Int where
  embed = show

-- |
-- >>> embed (7 :: Float)
-- "7.0"
instance Embed Float where
  embed = show

-- |
-- >>> embed 4.0
-- "4.0"
--
-- >>> embed (7 :: Double)
-- "7.0"
instance Embed Double where
  embed = show

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


-- | Construct shell call
quoteShellExp :: String -> Q Exp
quoteShellExp s = do
  shellEx <- runIO $ getEnvDefault "SHELL" "/bin/sh"
  [e| eval shellEx ["-c", $(string2exp s)] |]

-- | Parse references to Haskell variables
string2exp :: String -> Q Exp
string2exp = raw where
  raw ('\\':'$':xs) = [e| '$' : $(raw xs) |]
  raw ('$':'{':xs)  = [e| $(var xs) |]
  raw (x  :xs)      = [e| x : $(raw xs) |]
  raw []            = [e| [] |]

  var xs = case break (== '}') xs of
    (name, '}':ys) -> [e| embed $(return (VarE (mkName name))) ++ $(raw ys) |]
    (name, _)      -> fail $ "Bad variable pattern: ${" ++ name
