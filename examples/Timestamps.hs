{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Read lines from stdin and echo them in stderr, adding a timestamp to each
-- one.
--
-- To test this example, better redirect standard error to a file:
--
--    examples/Timestamps 2> /tmp/err.log
-- 
-- and check the file after having typed a few lines.
module Main (
        main
    ) where

import Prelude hiding (lines)

import Control.Monad
import qualified "text" Data.Text as T
import qualified "text" Data.Text.Encoding as T
import "time" Data.Time

import qualified "foldl" Control.Foldl as L
import "foldl-transduce" Control.Foldl.Transduce
import "foldl-transduce" Control.Foldl.Transduce.Text (utf8lenient,lines,newline)
import "foldl-transduce" Control.Foldl.Transduce.ByteString.IO (driveHandle,toHandle)

import System.IO

timestamp :: IO [T.Text]
timestamp = liftM (return . T.pack . formatTime defaultTimeLocale "%M m %S s | ") getCurrentTime 

main :: IO ()
main = 
    driveHandle (withEncodeDecode addTimestamps (toHandle stderr)) 2048 stdin
  where
    withEncodeDecode t f = 
        transduceM utf8lenient (t (L.premapM T.encodeUtf8 f))
    addTimestamps = groupsM lines (surroundIO timestamp (return ["\n"]))

