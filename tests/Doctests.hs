module Main where

import System.Environment (getArgs)
import Test.DocTest (doctest)


main :: IO ()
main = getArgs >>= doctest
