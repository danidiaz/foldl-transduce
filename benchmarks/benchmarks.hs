module Main where

import Data.Foldable
import qualified Control.Foldl as L
import Control.Foldl.Transduce
import Lens.Family

import Criterion.Main

main :: IO ()
main = defaultMain [
        bgroup "sum" [
             bench "without trans" (nf 
                (L.fold L.sum)
                (take 500000 (cycle [1::Int,-1])))
        ,
             bench "with trans"  (nf 
                (L.fold (folds (chunksOf 1) L.list (L.handles (folding toList) L.sum)))
                (take 500000 (cycle [1::Int,-1])))
        ]        
    ]
