{-# LANGUAGE PackageImports #-}

-- |
--
-- Count characters, words, lines and sentences in the file passed as commad
-- line argument.
module Main (
        main
    ) where

import Prelude hiding (words,lines,paragraphs)

import qualified "text" Data.Text as T

import qualified "foldl" Control.Foldl as L
import "foldl-transduce" Control.Foldl.Transduce
import "foldl-transduce" Control.Foldl.Transduce.Text (utf8lenient,words,lines,textualSplit)
import "foldl-transduce" Control.Foldl.Transduce.ByteString (drainHandle,chunkSizeDefault)

import System.IO
import System.Environment (getArgs)

multicount :: Fold T.Text (Int,Int,Int,Int)
multicount = 
    (,,,) <$> countchars <*> countwords <*> countlines <*> countsentences <*> countparagraps
  where
    countchars = L.premap T.length L.sum
    countwords = folds words unit L.length
    countlines = folds lines unit L.length
    countsentences = folds (textualSplit (=='.')) unit L.length
    countparagraps = folds paragraphs unit L.length

-- wget https://www.gutenberg.org/files/2650/2650-0.txt
main :: IO ()
main = do
    filename:_ <- getArgs 
    count <- withFile filename ReadMode $ 
        drainHandle (transduce utf8lenient multicount) chunkSizeDefault
    print count 
