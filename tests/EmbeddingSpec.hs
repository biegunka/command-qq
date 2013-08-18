{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module EmbeddingSpec where

import System.Command.QQ
import Test.Hspec


spec :: Spec
spec = do
  describe "simple variable embeddings" $ do
    it "embeds integers" $ do
      let n = 7
      [sh|echo #{n}|] `shouldReturn` "7\n"
    it "embeds doubles" $ do
      let n = 7.0
      [sh|echo #{n}|] `shouldReturn` "7.0\n"
    it "embeds characters" $ do
      let n = 'z'
      [sh|echo #{n}|] `shouldReturn` "z\n"
    it "embeds strings" $ do
      let n = "hello"
      [sh|echo #{n}|] `shouldReturn` "hello\n"
  describe "escapings" $ do
    it "is possible to write #{} in scripts still (as a comment)" $ do
      [sh|echo #\{n}|] `shouldReturn` "\n"
    it "is possible to write #{} in scripts still (as a string)" $ do
      [sh|echo "#\{n}"|] `shouldReturn` "#{n}\n"
      [sh|echo "#\\{n}"|] `shouldReturn` "#\\{n}\n"
