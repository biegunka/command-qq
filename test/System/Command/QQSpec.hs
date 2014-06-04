{-# LANGUAGE TemplateHaskell #-}
module System.Command.QQSpec (spec) where

import Test.Hspec

import System.Command.QQ (substituteVars)


spec :: Spec
spec = do
  describe "substituteVars" $ do
    it "does not change simple string literals" $
      $(substituteVars "hello!") `shouldBe` "hello!"

    it "substitutes variables of type Int" $ do
      let foo = 5 :: Int
      $(substituteVars "hello#{foo}bye!") `shouldBe` "hello5bye!"

    it "substitutes variables of type String" $ do
      let foo = "or"
      $(substituteVars "hello#{foo}bye!") `shouldBe` "helloorbye!"

    it "leaves an escape hatch for typing literal #{} in" $ do
      let foo = "or"
      $(substituteVars "hello#\\{foo}bye!") `shouldBe` "hello#{foo}bye!"

    it "escape hatch can be escaped" $ do
      let foo = "or"
      $(substituteVars "hello#\\\\{foo}bye!") `shouldBe` "hello#\\{foo}bye!"

    it "leaves another (possibly more natural) escape hatch for typing literal #{} in" $ do
      let foo = "or"
      $(substituteVars "hello\\#{foo}bye!") `shouldBe` "hello#{foo}bye!"

    it "another escape hatch can be escaped too" $ do
      let foo = "or"
      $(substituteVars "hello\\\\#{foo}bye!") `shouldBe` "hello\\orbye!"
