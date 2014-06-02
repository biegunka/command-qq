module System.Command.QQ.EmbedSpec (spec) where

import Data.Int
import Data.Ratio
import Data.Word
import Foreign.C.Types
import System.Command.QQ
import Test.Hspec


spec :: Spec
spec =
  describe "embedding" $ do
    describe "Haskell types" $ do
      describe "numeric types" $ do
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

        it "embeds Rational" $
          embed (3 % 5) `shouldBe` "0.6"

      it "embeds Char" $
        embed 'e' `shouldBe` "e"

      it "embeds String" $
        embed "foo" `shouldBe` "foo"

    describe "C types" $ do
      it "embeds CChar" $
        embed (4 :: CChar) `shouldBe` "4"

      it "embeds CSChar" $
        embed (4 :: CSChar) `shouldBe` "4"

      it "embeds CUChar" $
        embed (4 :: CUChar) `shouldBe` "4"

      it "embeds CShort" $
        embed (4 :: CShort) `shouldBe` "4"

      it "embeds CUShort" $
        embed (4 :: CUShort) `shouldBe` "4"

      it "embeds CInt" $
        embed (4 :: CInt) `shouldBe` "4"

      it "embeds CUInt" $
        embed (4 :: CUInt) `shouldBe` "4"

      it "embeds CLong" $
        embed (4 :: CLong) `shouldBe` "4"

      it "embeds CULong" $
        embed (4 :: CULong) `shouldBe` "4"

      it "embeds CSize" $
        embed (4 :: CSize) `shouldBe` "4"

      it "embeds CLLong" $
        embed (4 :: CLLong) `shouldBe` "4"

      it "embeds CULLong" $
        embed (4 :: CULLong) `shouldBe` "4"

      it "embeds CFloat" $
        embed (7 :: CFloat) `shouldBe` "7.0"

      it "embeds CDouble" $
        embed (7 :: CDouble) `shouldBe` "7.0"
