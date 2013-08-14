-- | Module to bypass GHC stage restrictions for quasiquoters
module ShellSpecQQ
  ( binsh, binbash
  ) where

import Language.Haskell.TH.Quote (QuasiQuoter)
import System.Shell.QQ (shell)


binsh, binbash :: QuasiQuoter
binsh   = shell "/bin/sh"
binbash = shell "/bin/bash"
