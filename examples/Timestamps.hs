{-# LANGUAGE PackageImports #-}

-- |
--
-- Count characters, words an lines of the file passed as commad line argument.
module Main (
        main
    ) where

import Prelude hiding (words,lines)

import Control.Monad
import qualified "text" Data.Text as T
import qualified "text" Data.Text.Encoding as T
import "time" Data.Time

import qualified "foldl" Control.Foldl as L
import "foldl-transduce" Control.Foldl.Transduce
import "foldl-transduce" Control.Foldl.Transduce.Text (utf8lenient,lines,newline)
import "foldl-transduce" Control.Foldl.Transduce.ByteString.IO (driveHandle,toHandle)

import System.IO
import System.Environment (getArgs)

timestamp :: IO [T.Text]
timestamp = liftM (return . T.pack . formatTime defaultTimeLocale "%M m %S s ") getCurrentTime 

main :: IO ()
main = 
  do
    driveHandle (transduceM utf8lenient (timestamped (L.premapM T.encodeUtf8 (toHandle stderr)))) 2048 stdin
  where
    timestamped = groupsM lines $ reifyM $ \f ->
        transduceM (surroundIO timestamp (return [])) (transduceM newline f)
        



