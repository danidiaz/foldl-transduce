{-# LANGUAGE PackageImports #-}

module Main (
        main
    ) where

import Prelude hiding (words,lines)

import qualified "text" Data.Text as T

import qualified "foldl" Control.Foldl as L
import "foldl-transduce" Control.Foldl.Transduce
import "foldl-transduce" Control.Foldl.Transduce.Text (utf8lenient,words,lines)
import "foldl-transduce" Control.Foldl.ByteString.IO (driveHandle)

import System.IO
import System.Environment (getArgs)

multicount :: Fold T.Text (Int,Int,Int)
multicount = (,,) <$> countchars <*> countwords <*> countlines
  where
    countchars = L.premap T.length L.sum
    countwords = folds words unit L.length
    countlines = folds lines unit L.length

-- wget https://www.gutenberg.org/files/2650/2650-0.txt
main :: IO ()
main = do
    filename:_ <- getArgs 
    count <- withFile filename ReadMode $ 
        driveHandle (L.generalize (transduce utf8lenient multicount)) 2048
    print count 
