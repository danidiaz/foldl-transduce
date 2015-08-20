{-# LANGUAGE OverloadedStrings #-}

module Control.Foldl.Transduce.Text (
        decoder
    ,   utf8
    ,   utf8lenient 
    ,   utf8strict
    ,   decoderE
    ,   utf8E
    ,   newline
    ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Exception.Base 
import qualified Control.Foldl as L
import qualified Control.Foldl.Transduce as L
import Control.Foldl.Transduce.Internal (Pair(..))
import qualified Control.Foldl.Text as T

decoder :: (B.ByteString -> T.Decoding) -> T.OnDecodeError -> L.Transducer B.ByteString T.Text ()
decoder _step onLeftovers = L.Transducer step (Pair mempty _step) done
    where
    step (Pair _ next) i = 
        let 
            T.Some txt leftovers next' = next i 
        in
        (Pair leftovers next', [txt])
    done (Pair leftovers _) = 
        if B.null leftovers
            then ((), [])
            else ((), foldMap (pure . T.singleton) onLeftovers')
    onLeftovers' = onLeftovers "leftovers" Nothing

utf8 :: T.OnDecodeError -> L.Transducer B.ByteString T.Text ()
utf8 onDecodeError = 
    decoder (T.streamDecodeUtf8With onDecodeError)  onDecodeError

utf8lenient :: L.Transducer B.ByteString T.Text ()
utf8lenient = utf8 T.lenientDecode

utf8strict :: L.Transducer B.ByteString T.Text ()
utf8strict = utf8 T.strictDecode

decoderE :: (T.OnDecodeError -> B.ByteString -> T.Decoding)
         -> L.TransducerM (ExceptT T.UnicodeException IO) B.ByteString T.Text ()   
decoderE next = L.TransducerM step (pure (Pair mempty next')) done
    where
        step (Pair _ next1) i = do
            emc <- liftIO . try . evaluate $ next1 i 
            case emc of 
                Left ue -> do
                    throwE ue
                Right (T.Some txt leftovers next2) -> do
                    return (Pair leftovers next2, [txt])
        done (Pair leftovers _) = do
            if B.null leftovers
                then return ((), [])
                else do
                    emc <- liftIO . try . evaluate $ onLeftovers'
                    case emc of
                        Left ue -> do
                            throwE ue
                        Right mc -> do
                            return ((), foldMap (pure . T.singleton) mc)
        next' = next T.strictDecode  
        onLeftovers' = T.strictDecode "leftovers" Nothing

utf8E :: L.TransducerM (ExceptT T.UnicodeException IO) B.ByteString T.Text ()   
utf8E = decoderE T.streamDecodeUtf8With


newline :: L.Transducer T.Text T.Text ()
newline = L.surround [] ["\n"]
