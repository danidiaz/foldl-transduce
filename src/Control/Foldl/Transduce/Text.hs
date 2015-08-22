{-# LANGUAGE OverloadedStrings #-}

module Control.Foldl.Transduce.Text (
        -- * Decoding transducers
        decoder
    ,   utf8
    ,   utf8lenient 
    ,   utf8strict
    ,   decoderE
    ,   utf8E
        -- * Other transducers
    ,   newline
    ,   stripStart
    ,   stripEnd
        -- * Splitters
    ,   lines
    ) where

import Prelude hiding (lines)
import Data.Char
import qualified Data.ByteString as B
import qualified Data.Text 
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

{- $setup

>>> import qualified Control.Foldl as L
>>> import Control.Foldl.Transduce

-}

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

{-| __/BEWARE!/__ 
    This pure function may throw 'UnicodeException'.
    __/BEWARE!/__ 

-}
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

{-| Appends a newline at the end of the stream.		

-}
newline :: L.Transducer T.Text T.Text ()
newline = L.surround [] ["\n"]

lines :: L.Splitter T.Text
lines = L.Splitter step False done 
    where
        step previousnl txt | Data.Text.null txt = (previousnl,[],[]) 
        step previousnl txt = do
            let
                lastc = Data.Text.last txt == '\n'
                txts = T.lines txt
            case (previousnl,txts) of
                (_,[]) -> error "never happens"
                (True,_) -> (lastc, [], map pure txts)
                (False,t:ts) -> (lastc, [t], map pure ts)
        done _ = []

blank :: T.Text -> Bool
blank = Data.Text.all isSpace

stripStart :: L.Transducer T.Text T.Text ()
stripStart = L.Transducer step False done
    where
        step True i = (True, [i])
        step False i =
            if blank i 
                then (False, [])
                else (True, [T.stripStart i])
        done _  = ((),[])

stripEnd :: L.Transducer T.Text T.Text ()
stripEnd = L.Transducer step [] done
    where
        step txts i =
            if blank i
                -- dangerous!
                then (i:txts, [])
                else ([i], reverse txts)
        done txts = case reverse txts of
            txt : _ -> ((), [T.stripEnd txt])
            _ -> ((), [])

