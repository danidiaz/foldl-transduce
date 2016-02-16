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
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
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

driveHandle :: (MonadIO m,ToFoldM m f) 
            => f B.ByteString r 
            -> Int -- ^ max chunk size
            -> Handle 
            -> m r 
driveHandle (toFoldM -> f) chunkSize handle = 
    L.impurely consumeFunc f (B.hGetSome handle chunkSize,hIsEOF handle)
    where
        -- adapted from foldM in Pipes.Prelude
        consumeFunc step begin done (readChunk,checkEOF) = do
            x0 <- begin
            loop x0
              where
                loop x = do
                    atEOF <- liftIO checkEOF
                    if atEOF 
                       then done x 
                       else do
                           chunk <- liftIO readChunk
                           x' <- step x chunk
                           loop $! x'


toHandle :: (MonadIO m) => Handle -> L.FoldM m B.ByteString ()
toHandle handle = 
    L.FoldM 
    (\_ b -> liftIO (B.hPut handle b))  
    (return ()) 
    (\_ -> return ())


toHandleBuilder :: (MonadIO m) => Handle -> L.FoldM m B.Builder ()
toHandleBuilder handle = 
    L.FoldM
    (\_ b -> liftIO (B.hPutBuilder handle b)) 
    (return ()) 
    (\_ -> return ())


