{-# LANGUAGE RankNTypes #-}

module Control.Foldl.ByteString.IO (
        driveHandle
    ,   toHandle
    ,   toHandleBuilder  
    ) where

import qualified Control.Foldl as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import Control.Monad.IO.Class
import System.IO

driveHandle :: MonadIO m => L.FoldM m B.ByteString r -> Int -> Handle -> m r 
driveHandle f chunkSize handle = L.impurely consumeFunc f (B.hGetSome handle chunkSize,hIsEOF handle)
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
