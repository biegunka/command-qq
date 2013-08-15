module Main where

import Test.DocTest (doctest)


main :: IO ()
main = doctest ["src/System/Shell/QQ.hs", "src/System/Shell/QQ/ShellT.hs"]
