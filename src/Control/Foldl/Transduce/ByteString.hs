{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
--
-- Pour handles into folds,
-- write to handles using folds. 
module Control.Foldl.Transduce.ByteString (
        -- * Reading from handles
        driveHandle
        -- * Writing to handles
    ,   toHandle
    ,   toHandleBuilder  
    ) where

import Control.Foldl.Transduce.ByteString.IO
