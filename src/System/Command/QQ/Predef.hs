-- | Some predefined quasiquoters
module System.Command.QQ.Predef where

import Language.Haskell.TH.Quote (QuasiQuoter)

import System.Command.QQ (interpreter, shell, quoter, callCommand)

-- | @bash@ shell
bash :: QuasiQuoter
bash = shell "bash"

-- | @zsh@ shell
zsh :: QuasiQuoter
zsh = shell "zsh"

-- | @awk@ interpreter
awk :: QuasiQuoter
awk = quoter $ callCommand "awk" []


-- | @ghci@ interpreter
ghci :: QuasiQuoter
ghci =
  quoter $ callCommand "ghc" ["-ignore-dot-ghci", "-e"]


-- | @perl@ interpreter
perl :: QuasiQuoter
perl = interpreter "perl"

-- | @ruby@ interpreter
ruby :: QuasiQuoter
ruby = interpreter "ruby"


-- | @python@ interpreter
python :: QuasiQuoter
python = shell "python"

-- | @python2@ interpreter
python2 :: QuasiQuoter
python2 = shell "python2"

-- | @python3@ interpreter
python3 :: QuasiQuoter
python3 = shell "python3"
