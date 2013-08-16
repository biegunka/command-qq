-- | Module to bypass GHC stage restrictions for quasiquoters
module CustomSpecQQ
  ( sh, bash
  , perl, ruby
  ) where

import           Language.Haskell.TH.Quote (QuasiQuoter)
import qualified System.Command.QQ as QQ


-- Shells
sh, bash :: QuasiQuoter
sh   = QQ.shell "sh"
bash = QQ.shell "bash"

-- Interpreters
perl, ruby :: QuasiQuoter
perl = QQ.interpreter "perl"
ruby = QQ.interpreter "ruby"
