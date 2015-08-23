module Main where

import Data.Monoid
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T

import qualified Control.Foldl as L
import Control.Foldl.Transduce
import Control.Foldl.Transduce.Text

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "Tests" 
    [
        testGroup "surround" 
        [
            testCase "surroundempty" $ 
                assertEqual mempty
                    "prefixsuffix"
                    (L.fold (transduce (surround "prefix" "suffix") L.list) "")
        ]
        ,
        testGroup "chunksOf" 
        [
            testCase "emptyList3" $ 
                assertEqual mempty
                    ([[]]::[[Int]])
                    (L.fold (folds (chunksOf 3) L.list L.list) [])
            ,
            testCase "size1" $ 
                assertEqual mempty
                    ([[1],[2],[3],[4],[5],[6],[7]]::[[Int]])
                    (L.fold (folds (chunksOf 1) L.list L.list) [1..7])
            ,
            testCase "size3" $ 
                assertEqual mempty
                    ([[1,2,3],[4,5,6],[7]]::[[Int]])
                    (L.fold (folds (chunksOf 3) L.list L.list) [1..7])
        ]
        ,
        testGroup "newline" $ 
        [
            testCase "newlineempty" $
                assertEqual mempty
                (T.pack "\n")
                (mconcat (L.fold (transduce newline L.list) (map T.pack [])))
            ,
            testCase "newlinenull" $
                assertEqual mempty
                (T.pack "\n")
                (mconcat (L.fold (transduce newline L.list) (map T.pack [""])))
        ]
    ]

