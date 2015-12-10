module Main where

import Prelude hiding (splitAt,lines,words)
import Data.Char
import Data.String hiding (lines,words)
import Data.Monoid
import Data.Bifunctor
import qualified Data.List (intersperse,splitAt)
import qualified Data.List.Split as Split
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

{- $quickcheck

   Notes for quickchecking on the REPL:

cabal repl tests
:t sample
sample :: Show a => Gen a -> IO ()
sample (arbitrary :: Gen WordA)

-}

main :: IO ()
main = defaultMain tests

testCaseEq :: (Eq a, Show a) => TestName -> a -> a -> TestTree
testCaseEq name a1 a2 = testCase name (assertEqual "" a1 a2)

blank :: T.Text -> Bool
blank = T.all isSpace

nl :: T.Text
nl = T.pack "\n"

sp :: T.Text
sp = T.pack " "

c :: T.Text
c = T.pack "c"

{- $words

-}

newtype WordA = WordA { getWord :: T.Text } deriving (Show)

instance Arbitrary WordA where
    arbitrary = do
        firstChar <- oneof [pure ' ', pure '\n', arbitrary]
        lastChar <- oneof [pure ' ', pure '\n', arbitrary]
        middle <- listOf (frequency [(1,pure ' '),(4,arbitrary)])
        return (WordA (T.pack (firstChar : (middle ++ [lastChar]))))

{- $paragraphs

-}

newtype TextChunksA = TextChunksA { getChunks :: [T.Text] } deriving (Show)

instance Arbitrary TextChunksA where
    arbitrary = flip suchThat (not . blank . mconcat . getChunks) (do
        TextChunksA <$> partz)
            where
                chunkz = frequency [
                      (20::Int, flip T.replicate sp <$> choose (1,40)) 
                    , (20, flip T.replicate sp <$> choose (1,3)) 
                    , (50, pure nl)
                    , (20, flip T.replicate c <$> choose (1,30))
                    , (20, flip T.replicate c <$> choose (1,3))
                    ]
                combined = mconcat <$> vectorOf 40 chunkz 
                partitions = infiniteListOf (choose (1::Int,7))
                partz = partition [] <$> combined <*> partitions
                partition :: [T.Text] -> T.Text -> [Int] -> [T.Text]
                partition accum text (x:xs) =
                    if x >= T.length text    
                       then reverse (text:accum)
                       else 
                           let (point,rest) = T.splitAt x text 
                           in 
                           partition (point:accum) rest xs
                partition _ _ [] = error "never happens"
    shrink (TextChunksA texts) = 
        let removeIndex i xs = 
                let (xs',xs'') = Data.List.splitAt i xs
                in xs' ++ tail xs'' 
            l = length texts
        in 
        if l == 1 
           then []
           else map (\i -> TextChunksA (removeIndex i texts)) [0..l-1] 

paragraphsBaseline 
    :: T.Text -> [T.Text] 
paragraphsBaseline =  
      map (T.unlines . map T.stripStart . T.lines)
    . map mconcat 
    . map (`mappend` [nl])
    . map (Data.List.intersperse nl)
    . filter (not . null) 
    . Split.splitWhen blank 
    . T.lines

ignoreLastNewline :: [T.Text] -> [T.Text]
ignoreLastNewline ts = 
    let 
        lastt = last ts
        lastt' = if T.last lastt == '\n' then T.init lastt else lastt 
    in init ts ++ [lastt']

-- (paragraphs,chunks)
splittedParagraphs :: T.Text -> [Int] -> [([T.Text],[T.Text])]
splittedParagraphs txt splitsizes =   
    let 
        splitted = paragraphsBaseline txt
    in 
    zip (repeat splitted) (map (flip T.chunksOf txt) splitsizes)

paragraphsUnderTest
    :: [T.Text] -> [T.Text] 
paragraphsUnderTest txt =
    map mconcat (L.fold (folds paragraphs L.list L.list) txt)

paragraph01 :: T.Text
paragraph01 = 
    T.pack 
    "  \n \n\n \n \n \
    \a aa aaa \nb bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb bb \n \
    \ ccccccccccccccccccccccc cccccccc \n\n  \n \n\n ccc\     
    \ \n \n \nd\n\n\ne \
    \\n" 
    
paragraph02 :: T.Text
paragraph02 = T.pack " cc  "

tests :: TestTree
tests = 
    testGroup "Tests" 
    [
        testGroup "surround" 
        [
            testCaseEq "surroundempty" 
                "prefixsuffix"
                (L.fold (transduce (surround "prefix" "suffix") L.list) "")
        ],
        testGroup "chunksOf" 
        [
            testCaseEq "emptyList3"
                ([[]]::[[Int]])
                (L.fold (folds (chunksOf 3) L.list L.list) [])
            ,
            testCaseEq "size1" 
                ([[1],[2],[3],[4],[5],[6],[7]]::[[Int]])
                (L.fold (folds (chunksOf 1) L.list L.list) [1..7])
            ,
            testCaseEq "size3" 
                ([[1,2,3],[4,5,6],[7]]::[[Int]])
                (L.fold (folds (chunksOf 3) L.list L.list) [1..7])
        ],
        testGroup "textualBreak"
        [
            testCaseEq "beginwithdot"
                ".bb"
                (L.fold (bisect (textualBreak (=='.')) ignore (reify id) L.mconcat) ["aa",".bb"])
            ,
            testCaseEq "endwithdot"
                "."
                (L.fold (bisect (textualBreak (=='.')) ignore (reify id) L.mconcat) ["aa","bb."])
        ],
        testGroup "newline"
        [
            testCaseEq "newlineempty"
                (T.pack "\n")
                (mconcat (L.fold (transduce newline L.list) (map T.pack [])))
            ,
            testCaseEq "newlinenull"
                (T.pack "\n")
                (mconcat (L.fold (transduce newline L.list) (map T.pack [""])))
        ],
        testGroup "words" 
        [ 
            testGroup "quickcheck" 
            [ 
                testProperty "quickcheck1" (\chunks -> -- list of words 
                    let tchunks = fmap getWord chunks 
                    in
                    (case TL.words (TL.fromChunks tchunks) of
                       [] -> [mempty]
                       x -> x) ==
                    (fmap TL.fromChunks (L.fold (folds words L.list L.list) tchunks)))
            ]
        ],
        testGroup "paragraphs" 
        [
            testCase "paragraphs01"
                (mapM_
                    (\(x,y) -> assertEqual "" (ignoreLastNewline x) (ignoreLastNewline (paragraphsUnderTest y))) 
                    (splittedParagraphs paragraph01 [1..7])),
            testCaseEq "newlineAtEnd"
                (map T.pack ["aa\n"]) 
                (paragraphsUnderTest (map T.pack ["a","a","\n"])),
            testCaseEq "noNewlineAtEnd"
                (map T.pack ["aa"]) 
                (paragraphsUnderTest (map T.pack ["a","a"])),
            testGroup "quickcheck" 
            [ 
                testProperty "quickcheck1" (\(TextChunksA chunks) ->
                        ignoreLastNewline (paragraphsUnderTest chunks)
                        ==
                        ignoreLastNewline (paragraphsBaseline (mconcat chunks)))
            ]
        ],
        testGroup "quiesceWith"  
        [
            testCase "collectAfterFailure" (do
               let
                  foldthatfails = 
                      transduceM utf8E (L.generalize L.list)
                  inputs = 
                      map fromString ["invalid \xc3\x28 sequence","xxx","zzz","___"]
                  fallbackfold =
                      bisectM (chunkedSplitAt 4) (reifyM id) ignore (L.generalize L.list)
               r <- L.foldM (quiesceWith fallbackfold foldthatfails) inputs 
               assertEqual 
                   mempty 
                   (Left ('C',4))
                   (first (bimap (head . show) (SFM.length . mconcat)) r))
        ]   
    ]

