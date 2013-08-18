-- | Custom quasiquoters built on top of command-qq
module CustomQQ
  ( sh, bash, zsh
  , python, perl, ruby, ghci
  ) where

import           Language.Haskell.TH.Quote -- template-haskell
import qualified System.Command.QQ as QQ   -- command-qq

-- $setup
-- >>> :set -XQuasiQuotes

-- | Some shell quasiquoters
--
-- >>> [sh|echo "`basename $0`"|] :: IO ()
-- sh
--
-- >>> [bash|echo "$(basename $0)"|] :: IO ()
-- bash
--
-- >>> [zsh|echo "$(basename $0)"|] :: IO ()
-- zsh
sh, bash, zsh :: QuasiQuoter
sh   = QQ.shell "sh"
bash = QQ.shell "bash"
zsh  = QQ.shell "zsh"

-- | Some interpreters quasiquoters
--
-- >>> let foo = 11
--
-- >>> [python|foo = 11; print foo == #{foo}|] :: IO ()
-- True
--
-- >>> [perl|my $foo = 11; print $foo == #{foo}|] :: IO ()
-- 1
--
-- >>> [ruby|foo = 11; puts foo == #{foo}|] :: IO ()
-- true
python, perl, ruby :: QuasiQuoter
python = QQ.shell "python" -- For whatever reason python uses @python -c <command>@ syntax
perl   = QQ.interpreter "perl"
ruby   = QQ.interpreter "ruby"

-- | More involved interpreter quasiquoter
--
-- >>> let foo = 11
-- >>> [ghci|let foo = 11 in print $ foo == #{foo}|] :: IO ()
-- True
ghci :: QuasiQuoter
ghci = QQ.quoter $ QQ.callCommand "ghc" ["-ignore-dot-ghci", "-e"]
