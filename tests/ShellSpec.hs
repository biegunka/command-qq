{-# LANGUAGE QuasiQuotes #-}
module ShellSpec where

import Test.Hspec

import ShellSpecQQ (binsh, binbash)


spec :: Spec
spec =
  describe "custom shell quasiquoters" $ do
    it "works correctly with /bin/sh quasiquoter" $
      [binsh|echo -n $0|] `shouldReturn` "/bin/sh"
    it "works correctly with /bin/bash quasiquoter" $
      [binbash|echo -n $0|] `shouldReturn` "/bin/bash"
