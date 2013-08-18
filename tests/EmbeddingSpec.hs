{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module EmbeddingSpec where

import System.Command.QQ
import Test.Hspec


spec :: Spec
spec = do
  describe "variable embeddings" $ do

    it "can embed integers" $
      let foo = 7 in [sh|echo #{foo}|] `shouldReturn` "7\n"

    it "can embed doubles" $
      let foo = 7.0 in [sh|echo #{foo}|] `shouldReturn` "7.0\n"

    it "can embed characters" $
      let foo = 'z' in [sh|echo #{foo}|] `shouldReturn` "z\n"

    it "can embed strings" $
      let foo = "hello" in [sh|echo #{foo}|] `shouldReturn` "hello\n"

  describe "multi-line embeddings" $ do

    it "supports multiline commands" $
      [sh|
        echo hello
        echo world
        echo !!!
      |] `shouldReturn` "hello\nworld\n!!!\n"

    it "supports embeddings in multiline commands" $
      let foo = 4
          bar = 7
      in [sh|
        echo #{foo}
        echo #{bar}
      |] `shouldReturn` "4\n7\n"

  describe "escapings" $ do

    it "is possible to write #{} in scripts still (as a comment)" $ do
      [sh|echo #\{foo}|] `shouldReturn` "\n"
      [sh|echo #\\{foo}|] `shouldReturn` "\n"

    it "is possible to write #{} in scripts still (as a string)" $ do
      [sh|echo "#\{foo}"|] `shouldReturn` "#{foo}\n"
      [sh|echo "#\\{foo}"|] `shouldReturn` "#\\{foo}\n"
