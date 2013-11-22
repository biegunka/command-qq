module System.Command.QQ.EmbedSpec (spec) where

import Data.Int
import Data.Word
import System.Command.QQ
import Test.Hspec


spec :: Spec
spec =
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
