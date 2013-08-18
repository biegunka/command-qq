module Main where

import Test.DocTest (doctest)


main :: IO ()
main = doctest
  [ "src/System/Command/QQ.hs"
  , "examples/CustomQQ.hs"
  , "examples/CommandT.hs"
  ]
