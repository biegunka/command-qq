{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module System.Command.QQ.PredefSpec (spec) where

import           Control.Applicative
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           System.Exit (ExitCode(..))
import           Test.Hspec

import           System.Command.QQ.Predef


spec :: Spec
spec = do
  it "runs bash" $
    stdout <$> [bash|echo $0|] `shouldReturn` "bash\n"

  it "runs zsh" $
    stdout <$> [zsh|echo $0|] `shouldReturn` "zsh\n"

  it "runs awk interpreter" $
    stdout <$> [awk|BEGIN { print 4 * 7 } |] `shouldReturn` "28\n"

  it "runs ghci" $
    stdout <$> [ghci|print (4 * 7)|] `shouldReturn` "28\n"

  it "runs perl interpreter" $
    stdout <$> [perl|print 4 * 7 . "\n"|] `shouldReturn` "28\n"

  it "runs ruby interpreter" $
    stdout <$> [ruby|puts 4 * 7|] `shouldReturn` "28\n"

  let traceback = "Traceback (most recent call last):"

  it "python shows tracebacks" $ do
    err <- Text.lines . stderr <$> [python|print(4 / 0)|]
    err `shouldContain` [traceback]

  it "python2 shows tracebacks" $ do
    err <- Text.lines . stderr <$> [python2|print(4 / 0)|]
    err `shouldContain` [traceback]

  it "python3 shows tracebacks" $ do
    err <- Text.lines . stderr <$> [python3|print(4 / 0)|]
    err `shouldContain` [traceback]

  it "python3 really runs python 3" $ do
    err <- Text.lines . stderr <$> [python3|print 7|]
    err `shouldContain` ["SyntaxError: invalid syntax"]


stdout, stderr :: (ExitCode, Text, Text) -> Text
stderr (_, _, xs) = xs
stdout (_, xs, _) = xs
