module Main where

import Prelude hiding (splitAt,lines,words)
import Data.String hiding (lines,words)
import Data.Monoid
import Data.Bifunctor
import qualified Data.Monoid.Factorial as SFM
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Control.Foldl as L
import Control.Foldl.Transduce
import Control.Foldl.Transduce.Text
import Control.Foldl.Transduce.Textual

main :: IO ()
main = defaultMain tests

newtype WordQC = WordQC { getWordQC :: T.Text } deriving (Show)

instance Arbitrary WordQC where
    arbitrary = do
        firstChar <- oneof [pure ' ', pure '\n', arbitrary]
        lastChar <- oneof [pure ' ', pure '\n', arbitrary]
        middle <- listOf (frequency [(1,pure ' '),(4,arbitrary)])
        return (WordQC (T.pack (firstChar : (middle ++ [lastChar]))))

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
        testGroup "textualBreak" $ 
        [
            testCase "beginwithdot" $
                assertEqual mempty
                    ".bb"
                    (L.fold (bisect (textualBreak (=='.')) ignore (reify id) L.mconcat) ["aa",".bb"])
            ,
            testCase "endwithdot" $
                assertEqual mempty
                    "."
                    (L.fold (bisect (textualBreak (=='.')) ignore (reify id) L.mconcat) ["aa","bb."])
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
        testGroup "words" 
        [ 
            testGroup "quickcheck" 
            [ 
                testProperty "quickcheck1" (\chunks -> 
                         let tchunks = fmap getWordQC chunks 
                         in
                         (case TL.words (TL.fromChunks tchunks) of
                            [] -> [mempty]
                            x -> x) ==
                         (fmap TL.fromChunks (L.fold (folds words L.list L.list) tchunks)))
            ]
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
                        bisectM (chunkedSplitAt 4) (reifyM id) ignore (L.generalize L.list)
                in
                do
                   r <- L.foldM (quiesceWith fallbackfold foldthatfails) inputs 
                   assertEqual 
                       mempty 
                       (Left ('C',4))
                       (first (bimap (head . show) (SFM.length . mconcat)) r)
        ]   
    ]

