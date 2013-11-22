{-# LANGUAGE FlexibleInstances #-}
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
  , module System.Command.QQ.Eval
  ) where

import Control.Applicative
import Data.Char (isLower, isUpper)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import System.Command.QQ.Embed
import System.Command.QQ.Eval

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings
-- >>> import System.Exit
-- >>> import Data.Text.Lazy (Text)


-- | Quasiquoter for the default shell
--
-- Constructs polymorphic action of type @Eval a => a@ from passed string.
--
-- Uses @SHELL@ environment variable as path to shell executable
-- or @\/bin\/sh@ if it is unset.
--
-- >>> [sh|echo "hello, world!"|] :: IO ExitCode
-- ExitSuccess
-- >>> [sh|echo "hello, world!"|] :: IO Text
-- "hello, world!\n"
--
-- Haskell values can be embedded with Ruby-like syntax:
--
-- >>> let apples = 7
-- >>> [sh|echo "#{apples} apples!"|] :: IO Text
-- "7 apples!\n"
sh :: QuasiQuoter
sh = quoter $ \string -> do
  shellEx <- runIO $ getEnvDefault "/bin/sh" "SHELL"
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
  shellEx <- runIO $ getEnvDefault "/bin/sh" "SHELL"
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
  raw, var :: String -> Q Exp
  raw (break (== '#') -> parts) = case parts of
    (before, '#':'{' :after) -> [e| before ++ $(var after) |]
    (before, '#':'\\':after) -> [e| before ++ '#' : $(raw after) |]
    (before, '#':after)      -> [e| before ++ '#' : $(raw after) |]
    (before, [])             -> [e| before |]
    _                        -> fail "Should never happen"

  var (break (== '}') -> parts) = case parts of
     (b:efore, '}':after)
        | isLower b                    -> external (VarE (mkName (b:efore))) after
        | isUpper b                    -> external (ConE (mkName (b:efore))) after
        | isUpper b                    -> external (ConE (mkName (b:efore))) after
        | Just i <- readMaybe (b:efore) -> external (LitE (IntegerL i)) after
        | Just d <- readMaybe (b:efore) -> external (LitE (RationalL (toRational (d :: Double)))) after
        | Just c <- readMaybe (b:efore) -> external (LitE (CharL c)) after
        | Just s <- readMaybe (b:efore) -> external (LitE (StringL s)) after
     (before, _)    -> fail $ "Invalid name: " ++ before

  external :: Exp -> String -> Q Exp
  external e after = [e| embed $(return e) ++ $(raw after) |]

-- | Get environment variable or default value if it's unset
getEnvDefault
  :: String -- ^ The default vefault
  -> String -- ^ Environment variable
  -> IO String
getEnvDefault def query = fromMaybe def <$> lookupEnv query
