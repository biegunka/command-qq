{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
module System.Command.QQ.EmbedSpec (spec) where

import Data.Int
import Data.Text.Lazy (Text, pack)
import Data.Word
import System.Command.QQ
import Test.Hspec


spec :: Spec
spec = do
  describe "embedding" $ do

    it "embeds Integer" $
      embed (4 :: Integer) `shouldBe` "4"

    it "embeds Int" $
      embed (4 :: Int) `shouldBe` "4"

    it "embeds Int8" $
      embed (4 :: Int8) `shouldBe` "4"

    it "embeds Int16" $
      embed (4 :: Int16) `shouldBe` "4"

    it "embeds Int32" $
      embed (4 :: Int32) `shouldBe` "4"

    it "embeds Int64" $
      embed (4 :: Int64) `shouldBe` "4"

    it "embeds Word" $
      embed (4 :: Word) `shouldBe` "4"

    it "embeds Word8" $
      embed (4 :: Word8) `shouldBe` "4"

    it "embeds Word16" $
      embed (4 :: Word16) `shouldBe` "4"

    it "embeds Word32" $
      embed (4 :: Word32) `shouldBe` "4"

    it "embeds Word64" $
      embed (4 :: Word64) `shouldBe` "4"

    it "embeds Float" $
      embed (7 :: Float) `shouldBe` "7.0"

    it "embeds Double" $
      embed (7 :: Double) `shouldBe` "7.0"

    it "embeds Char" $
      embed 'e' `shouldBe` "e"

    it "embeds String" $
      embed "foo" `shouldBe` "foo"

  describe "quasiquotation" $ do
    describe "variable embeddings" $ do

      it "can embed integers" $
        let foo = 7 in [sh|echo #{foo}|] `shouldReturn` text "7\n"

      it "can embed doubles" $
        let foo = 7.0 in [sh|echo #{foo}|] `shouldReturn` text "7.0\n"

      it "can embed characters" $
        let foo = 'z' in [sh|echo #{foo}|] `shouldReturn` text "z\n"

      it "can embed strings" $
        let foo = "hello" in [sh|echo #{foo}|] `shouldReturn` text "hello\n"

    describe "multi-line embeddings" $ do

      it "supports multiline commands" $
        [sh|
          echo hello
          echo world
          echo !!!
        |] `shouldReturn` text "hello\nworld\n!!!\n"

      it "supports embeddings in multiline commands" $
        let foo = 4
            bar = 7
        in [sh|
          echo #{foo}
          echo #{bar}
        |] `shouldReturn` text "4\n7\n"

    describe "escapings" $ do

      it "is possible to write #{} in scripts still (as a comment)" $ do
        [sh|echo #\{foo}|] `shouldReturn` text "\n"
        [sh|echo #\\{foo}|] `shouldReturn` text "\n"

      it "is possible to write #{} in scripts still (as a string)" $ do
        [sh|echo "#\{foo}"|] `shouldReturn` text "#{foo}\n"
        [sh|echo "#\\{foo}"|] `shouldReturn` text "#\\{foo}\n"

text :: String -> Text
text = pack
