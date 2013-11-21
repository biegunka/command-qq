{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Example DSL made on top of "System.Command.QQ"
--
-- Provides [semi-]convenient way to run external commands
-- in sequence and parse their output.
--
-- DSL does not use any custom quasiquoters but provides 'Eval'
-- instances for custom datatypes that implement desired semantics
module CommandT where

import Control.Applicative                         -- base
  ( Applicative(..), Alternative(..) )
import Control.Monad (MonadPlus(..))               -- base
import Control.Monad.IO.Class (MonadIO(..))        -- transformers
import Control.Monad.Trans.Class (MonadTrans(..))  -- transformers
import Control.Monad.Trans.Either                  -- either
import Data.Monoid (Last(..))                      -- base
import Data.Text.Lazy (Text)                       -- text
import System.Exit (ExitCode(..))                  -- base
import System.Command.QQ (Eval(..))                -- command-qq

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import System.Command.QQ
-- >>> import qualified Data.Text.Lazy as T
-- >>> let lengths = [sh|while read line; do echo ${#line}; done|] :: Text -> CommandT IO Text


infixl 1 >>! -- same as >>=


-- | External commands sequencing result
--
-- Every external command results either in failure (thus provides non-zero
-- exit code and @stderr@) or some value (typically its @stdout@)
--
-- For example:
--
-- >>> runCommandT $ [sh|echo -e "hello\nworld!!!"|] >>= lengths
-- Right "5\n8\n"
--
-- 'CommandT' implements the usual 'Alternative' instance semantics:
--
-- >>> runCommandT $ [sh|exit 1|] <|> [sh|echo hello|]
-- Right "hello\n"
--
-- If everything fails, then last failure is returned:
--
-- >>> do Left (Last (Just (Failure _ i _))) <- runCommandT $ [sh|exit 1|] <|> [sh|exit 3|]; print i
-- 3
newtype CommandT m a = CommandT { unCommandT :: EitherT (Last Failure) m a }

-- | Failed command with exit code and @stderr@
data Failure = Failure Command Int Text
    deriving (Show, Read)

-- | Command name and arguments
data Command = Command String [String]
    deriving (Show, Read)

-- | Run external commands and get the result
runCommandT :: CommandT m a -> m (Either (Last Failure) a)
runCommandT = runEitherT . unCommandT

instance Monad m => Functor (CommandT m) where
  fmap f (CommandT x) = CommandT (fmap f x)

instance Monad m => Applicative (CommandT m) where
  pure = CommandT . pure
  CommandT f <*> CommandT x = CommandT (f <*> x)

instance Monad m => Monad (CommandT m) where
  return = pure
  CommandT x >>= k = CommandT (x >>= unCommandT . k)

instance Monad m => Alternative (CommandT m) where
  empty = CommandT empty
  CommandT f <|> CommandT x = CommandT (f <|> x)

instance Monad m => MonadPlus (CommandT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans CommandT where
  lift = CommandT . lift

instance MonadIO m => MonadIO (CommandT m) where
  liftIO = lift . liftIO

instance (o ~ Text, MonadIO m) => Eval (CommandT m o) where
  eval command args = CommandT . EitherT $ do
    (status, out, err) <- liftIO $ eval command args
    return $ case status of
      ExitSuccess   -> Right out
      ExitFailure i -> Left (Last (Just (Failure (Command command args) i err)))

instance (i ~ Text, o ~ Text, MonadIO m) => Eval (i -> CommandT m o) where
  eval command args input = CommandT . EitherT $ do
    (status, out, err) <- liftIO $ eval command args input
    return $ case status of
      ExitSuccess   -> Right out
      ExitFailure i -> Left (Last (Just (Failure (Command command args) i err)))


-- | Pass @stderr@ of failed external command to function
--
-- If nothing has failed, we do not have @stderr@, really:
--
-- >>> runCommandT $ [sh|echo -e "hello\nworld!!!">&2|] >>! lengths
-- Left (Last {getLast = Nothing})
--
-- If something has failed, we do have @stderr@ to play with:
--
-- >>> runCommandT $ [sh|echo -e "hello\nworld!!!">&2; exit 1|] >>! lengths
-- Right "5\n8\n"
--
-- And playing may involve arbitrary Haskell functions, of course:
--
-- >>> runCommandT $ [sh|echo -e "hello\nworld!!!">&2; exit 1|] >>! lengths . T.unlines . reverse . T.lines
-- Right "8\n5\n"
(>>!) :: Monad m => CommandT m a -> (Text -> CommandT m b) -> CommandT m b
x >>! k = CommandT . EitherT $ do
  t <- runCommandT x
  case t of
    Left (Last Nothing) -> return (Left (Last Nothing))
    Left (Last (Just (Failure _ _ err))) -> runCommandT $ k err
    Right _ -> return (Left (Last Nothing))
