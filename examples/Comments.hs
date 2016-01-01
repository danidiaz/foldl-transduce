{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Removing comments from a string.  This is a very basic example that doesn't
-- handle nested comments, or quoted comments.

module Main (
        main
    ) where

import Data.Bool
import qualified "text" Data.Text as T
import qualified "text" Data.Text.IO as T

import qualified "foldl" Control.Foldl as L
import "foldl-transduce" Control.Foldl.Transduce (transduce,ignore,groups,Moore(..),unfold,reify)
import "foldl-transduce" Control.Foldl.Transduce.Text (sections)

textWithComments :: [T.Text]
textWithComments = [
      "foo"
    , "{- hi"
    , "there -}"  
    , "bar"
    , "{- another"
    , "comment -}"
    , "baz"
    ]

removeComments :: L.Fold T.Text T.Text
removeComments =
    groups sectionSplitter ignoreAlternating L.mconcat
  where
    sectionSplitter = sections (cycle ["{-","-}"])
    ignoreAlternating = Moore $  
        unfold 
        (\ts -> (head ts, \_ -> tail ts)) 
        (cycle [reify id,reify (transduce ignore)])

main :: IO ()
main = do
    T.putStrLn (L.fold removeComments textWithComments)
