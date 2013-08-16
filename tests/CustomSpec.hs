{-# LANGUAGE QuasiQuotes #-}
module CustomSpec where

import Test.Hspec

import CustomSpecQQ (sh, bash, ruby, perl)


spec :: Spec
spec =
  describe "custom shell quasiquoters" $ do
    context "shells" $ do
      it "works correctly with /bin/sh quasiquoter" $ do
        [sh|echo $0|] `shouldReturn` "sh\n"
      it "works correctly with /bin/bash quasiquoter" $
        [bash|echo $0|] `shouldReturn` "bash\n"
    context "interpreters" $ do
      it "works correctly with /usr/bin/perl quasiquoter" $
        [perl|print "perl" . "\n"|] `shouldReturn` "perl\n"
      it "works correctly with /usr/bin/ruby quasiquoter" $
        [ruby|puts "ruby"|] `shouldReturn` "ruby\n"
