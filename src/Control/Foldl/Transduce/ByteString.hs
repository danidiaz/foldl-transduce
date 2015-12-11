{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- Pour handles into folds,
-- write to handles using folds. 
module Control.Foldl.Transduce.ByteString (
        -- * Reading from handles
        drainHandle
    ,   ChunkSize
    ,   chunkSize
    ,   chunkSizeDefault
        -- * Writing to handles
    ,   toHandle
    ,   toHandleBuilder  
    ) where

import qualified Control.Foldl as L
import Control.Foldl.Transduce 
import Control.Foldl.Transduce.ByteString.IO
import qualified Data.ByteString as B
import Control.Monad.IO.Class
import System.IO
import Data.ByteString.Lazy.Internal (defaultChunkSize)

{-| Feed a fold with bytes read from a 'Handle'.

-}
drainHandle 
    :: (MonadIO m,ToFoldM m f) 
    => f B.ByteString r 
    -> ChunkSize 
    -> Handle 
    -> m r 
drainHandle f (ChunkSize csize) h = driveHandle f csize h

{-| Maximum chunk size		

-}
newtype ChunkSize = ChunkSize Int deriving (Show,Eq,Ord,Num)

chunkSize :: Int -> ChunkSize
chunkSize = ChunkSize

chunkSizeDefault :: ChunkSize
chunkSizeDefault = chunkSize defaultChunkSize
