module Main where

import Test.DocTest

main :: IO ()
main = doctest 
    [
        "src/Control/Foldl/Transduce.hs",
        "src/Control/Foldl/Transduce/Textual.hs",
        "src/Control/Foldl/Transduce/Text.hs"
    ]
