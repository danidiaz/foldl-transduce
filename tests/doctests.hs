module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Control/Foldl/Transduce.hs"]