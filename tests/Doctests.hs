module Main where

import Test.DocTest (doctest)


main :: IO ()
main = doctest
  [ "src/System/Command/QQ.hs"
  , "src/System/Command/QQ/CommandT.hs"
  ]
