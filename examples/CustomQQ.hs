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
-- >>> let n = 11
--
-- >>> [python|n = 11; print n == #{n}|] :: IO ()
-- True
--
-- >>> [perl|my $n = 11; print $n == #{n}|] :: IO ()
-- 1
--
-- >>> [ruby|n = 11; puts n == #{n}|] :: IO ()
-- true
python, perl, ruby :: QuasiQuoter
python = QQ.shell "python" -- For whatever reason python uses @python -c <command>@ syntax
perl   = QQ.interpreter "perl"
ruby   = QQ.interpreter "ruby"

-- | More involved interpreter quasiquoter
--
-- >>> let n = 11
-- >>> [ghci|let n = 11 in print $ n == #{n}|] :: IO ()
-- True
ghci :: QuasiQuoter
ghci = QQ.quoter $ QQ.callCommand "ghc" ["-ignore-dot-ghci", "-e"]
