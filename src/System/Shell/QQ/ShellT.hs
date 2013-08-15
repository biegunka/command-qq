{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Example DSL made on top of "System.Shell.QQ"
--
-- Provides [semi-]convenient way to run shell commands and parse
-- their output.
--
-- DSL does not use any custom quasiquoters but provides 'Eval'
-- instances for custom datatypes that implement desired semantics
module System.Shell.QQ.ShellT where

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either
import Data.Monoid (Last(..))
import System.Exit (ExitCode(..))
import System.Shell.QQ (Eval(..))

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import System.Shell.QQ
-- >>> let lengths = [sh|while read line; do echo ${#line}; done|] :: String -> ShellT IO String


infixl 1 >>!


-- | Shell commands sequencing result
--
-- Every shell command results either in failure (thus provides non-zero
-- exit code and @stderr@) or some value (typically its @stdout@)
--
-- For example:
--
-- >>> runShellT $ [sh|echo -e "hello\nworld!!!"|] >>= lengths
-- Right "5\n8\n"
--
-- 'ShellT' implements the usual 'Alternative' instance semantics:
--
-- >>> runShellT $ [sh|exit 1|] <|> [sh|echo hello|]
-- Right "hello\n"
--
-- If everything fails, then last failure is returned:
--
-- >>> do Left (Last (Just (Failure _ i _))) <- runShellT $ [sh|exit 1|] <|> [sh|exit 3|]; print i
-- 3
newtype ShellT m a = ShellT { unShellT :: EitherT (Last Failure) m a }

-- | Failed command with exit code and @stderr@
data Failure = Failure Command Int String
    deriving (Show, Read)

-- | Command name and arguments
data Command = Command String [String]
    deriving (Show, Read)

-- | Run shell commands and get the result
runShellT :: ShellT m a -> m (Either (Last Failure) a)
runShellT = runEitherT . unShellT

instance Monad m => Functor (ShellT m) where
  fmap f (ShellT x) = ShellT (fmap f x)

instance Monad m => Applicative (ShellT m) where
  pure = ShellT . pure
  ShellT f <*> ShellT x = ShellT (f <*> x)

instance Monad m => Monad (ShellT m) where
  return = pure
  ShellT x >>= k = ShellT (x >>= unShellT . k)

instance Monad m => Alternative (ShellT m) where
  empty = ShellT empty
  ShellT f <|> ShellT x = ShellT (f <|> x)

instance Monad m => MonadPlus (ShellT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans ShellT where
  lift = ShellT . lift

instance MonadIO m => MonadIO (ShellT m) where
  liftIO = lift . liftIO

instance (o ~ String, MonadIO m) => Eval (ShellT m o) where
  eval command args = ShellT . EitherT $ do
    (status, out, err) <- liftIO $ eval command args
    return $ case status of
      ExitSuccess   -> Right out
      ExitFailure i -> Left (Last (Just (Failure (Command command args) i err)))

instance (i ~ String, o ~ String, MonadIO m) => Eval (i -> ShellT m o) where
  eval command args input = ShellT . EitherT $ do
    (status, out, err) <- liftIO $ eval command args input
    return $ case status of
      ExitSuccess   -> Right out
      ExitFailure i -> Left (Last (Just (Failure (Command command args) i err)))


-- | Pass @stderr@ of failed shell command to function
--
-- If nothing has failed, we do not have @stderr@, really:
--
-- >>> runShellT $ [sh|echo -e "hello\nworld!!!">&2|] >>! lengths
-- Left (Last {getLast = Nothing})
--
-- If something has failed, we do have @stderr@ to play with:
--
-- >>> runShellT $ [sh|echo -e "hello\nworld!!!">&2; exit 1|] >>! lengths
-- Right "5\n8\n"
--
-- And playing may involve arbitrary Haskell functions, of course:
--
-- >>> runShellT $ [sh|echo -e "hello\nworld!!!">&2; exit 1|] >>! lengths . unlines . reverse . lines
-- Right "8\n5\n"
(>>!) :: Monad m => ShellT m a -> (String -> ShellT m b) -> ShellT m b
x >>! k = ShellT . EitherT $ do
  t <- runShellT x
  case t of
    Left (Last Nothing) -> return (Left (Last Nothing))
    Left (Last (Just (Failure _ _ err))) -> runShellT $ k err
    Right _ -> return (Left (Last Nothing))
