module Main where

import Test.DocTest (doctest)


main :: IO ()
main = doctest
  [ "src/System/Command/QQ.hs"
  , "src/System/Command/QQ/Embed.hs"
  , "src/System/Command/QQ/Eval.hs"
  , "src/System/Command/QQ/Predef.hs"
  , "example/CommandT.hs"
  ]
