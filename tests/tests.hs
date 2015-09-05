module Main where

import Prelude hiding (splitAt)
import Data.String hiding (lines,words)
import Data.Monoid
import Data.Bifunctor
import qualified Data.Monoid.Factorial as SFM
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
        ,
        testGroup "quiesceWith" $ 
        [
            testCase "collectAfterFailure" $
                let foldthatfails = 
                        transduceM utf8E (L.generalize L.list)
                    inputs = 
                        map fromString ["invalid \xc3\x28 sequence","xxx","zzz","___"]
                    fallbackfold =
                        bisectM (chunkedSplitAt 4) id (transduceM ignore) (L.generalize L.list)
                in
                do
                   r <- L.foldM (quiesceWith fallbackfold foldthatfails) inputs 
                   assertEqual 
                       mempty 
                       (Left ('C',4))
                       (first (bimap (head . show) (SFM.length . mconcat)) r)
        ]   
    ]

