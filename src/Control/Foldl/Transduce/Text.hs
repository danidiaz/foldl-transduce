module Control.Foldl.Transduce.Text (
        decoder
    ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Control.Foldl as L
import qualified Control.Foldl.Transduce as L
import qualified Control.Foldl.Text as T

decoder :: (B.ByteString -> T.Decoding) -> L.Transducer B.ByteString T.Text ()
decoder = undefined
