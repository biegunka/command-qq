{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
module System.Command.QQ.EvalSpec (spec) where

import Data.Text.Lazy (Text, pack)
import System.Command.QQ
import System.Exit (ExitCode(..))
import Test.Hspec


spec :: Spec
spec = do
  describe "quasiquotation" $ do
    context "unicode" $ do
      it "works with unicode symbols in the String output" $
        [sh|echo -n "ДМИТРИЙ МАЛИКОВ"|] `shouldReturn` "ДМИТРИЙ МАЛИКОВ"

      it "works with unicode symbols in the Text output" $
        [sh|echo -n "ЕГОР ЛЕТОВ"|] `shouldReturn` text "ЕГОР ЛЕТОВ"

    context "exit code" $ do
      it "is possible to get successful exit code" $
        [sh|exit 0|] `shouldReturn` ExitSuccess

      it "is possible to get unsuccessful exit code" $
        [sh|exit 4|] `shouldReturn` ExitFailure 4

    it "is possible to get all of exitcode, stdout, and stderr" $
      [sh|echo -n hello; echo -n world >&2; exit 4|] `shouldReturn`
        (ExitFailure 4, text "hello", text "world")

  describe "embedding" $ do
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
      it "is possible to write #{} literally as a comment" $ do
        [sh|echo #\{foo}|] `shouldReturn` "\n"
        [sh|echo #\\{foo}|] `shouldReturn` "\n"

      it "is possible to write #{} literally as a string" $ do
        [sh|echo "#\{foo}"|] `shouldReturn` "#{foo}\n"
        [sh|echo "#\\{foo}"|] `shouldReturn` "#\\{foo}\n"

text :: String -> Text
text = pack
