{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- This module builds on module "Control.Foldl.Text", adding stateful
-- transducers and grouping operations.
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

>>> import Data.String hiding (lines)
>>> import Data.Text (Text)
>>> import Control.Monad.Trans.Except
>>> import qualified Control.Foldl as L
>>> import Control.Foldl.Transduce

-}

{-| Builds a decoding 'Transducer' out of a stream-oriented decoding function
    from "Data.Text.Encoding" and an error handler from
    "Data.Text.Encoding.Error".		

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

{-| Builds a UTF8-decoding 'Transducer'. Takes an error handler from
    'Data.Text.Encoding.Error'.		

-}
utf8 :: T.OnDecodeError -> L.Transducer B.ByteString T.Text ()
utf8 onDecodeError = 
    decoder (T.streamDecodeUtf8With onDecodeError)  onDecodeError

{-| UTF8-decoding 'Transducer' that replaces invalid input bytes with the
    Unicode replacement character U+FFFD.

>>> L.fold (transduce utf8lenient L.list) (map fromString ["decode","this"])
["decode","this"]

>>> L.fold (transduce utf8lenient L.list) (map fromString ["across \xe2","\x98\x83 boundaries"])
["across ","\9731 boundaries"]

>>> L.fold (transduce utf8lenient L.list) (map fromString ["invalid \xc3\x28 sequence"])
["invalid \65533 sequence"]

>>> L.fold (transduce utf8lenient L.list) (map fromString ["incomplete \xe2"])
["incomplete ","\65533"]
-}
utf8lenient :: L.Transducer B.ByteString T.Text ()
utf8lenient = utf8 T.lenientDecode

{-| __/BEWARE!/__ 
    This 'Transducer' may throw 'UnicodeException'.
    __/BEWARE!/__ 

>>> L.fold (transduce utf8strict L.list) (map fromString ["invalid \xc3\x28 sequence"])
*** Exception: Cannot decode byte '\x28': Data.Text.Internal.Encoding.streamDecodeUtf8With: Invalid UTF-8 stream

>>> L.fold (transduce utf8strict L.list) (map fromString ["incomplete \xe2"])
*** Exception: Cannot decode input: leftovers
-}
utf8strict :: L.Transducer B.ByteString T.Text ()
utf8strict = utf8 T.strictDecode

{-| Similar to 'decoder', but catches 'UnicodeException' in 'IO' and uses
    'Control.Monad.Trans.Except' to communicate the error.		

-}
decoderE :: MonadIO m
         => (T.OnDecodeError -> B.ByteString -> T.Decoding)
         -> L.TransducerM (ExceptT T.UnicodeException m) B.ByteString T.Text ()   
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

{-| Like 'utf8strict', but catches 'UnicodeException' in 'IO' and uses
    'Control.Monad.Trans.Except' to communicate the error.		

>>> runExceptT $ L.foldM (transduceM utf8E (L.generalize L.list)) (map fromString ["invalid \xc3\x28 sequence"])
Left Cannot decode byte '\x28': Data.Text.Internal.Encoding.streamDecodeUtf8With: Invalid UTF-8 stream

>>> runExceptT $ L.foldM (transduceM utf8E (L.generalize L.list)) (map fromString ["incomplete \xe2"])
Left Cannot decode input: leftovers
-}
utf8E :: MonadIO m => L.TransducerM (ExceptT T.UnicodeException m) B.ByteString T.Text ()   
utf8E = decoderE T.streamDecodeUtf8With

{-| Appends a newline at the end of the stream.		

>>> L.fold (transduce newline L.list) (map T.pack ["without","newline"])
["without","newline","\n"]
-}
newline :: L.Transducer T.Text T.Text ()
newline = L.surround [] ["\n"]

blank :: T.Text -> Bool
blank = Data.Text.all isSpace

{-| Remove leading white space from a stream of 'Text'.		

>>> L.fold (transduce stripStart L.list) (map T.pack ["   ","", "   text "])
["text "]
-}
stripStart :: L.Transducer T.Text T.Text ()
stripStart = L.Transducer step False done
    where
        step True i = (True, [i])
        step False i =
            if blank i 
                then (False, [])
                else (True, [T.stripStart i])
        done _  = ((),[])

{-| Remove trailing white space from a stream of 'Text'.		

    __/BEWARE!/__ 
    This function naively accumulates in memory any arriving "blank blocks" of
    text until a non-blank block or end-of-stream arrives, and therefore it is
    potentially dangerous. Do not use with untrusted inputs.

>>> L.fold (transduce stripEnd L.list) (map T.pack [" ", "   text ", "   ", "" , " "])
[" ","   text"]
-}
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

{-| Splits a stream into lines, removing the newlines.

>>> L.fold (L.groups lines id L.list) (map T.pack ["line 1\n line 2\n"])
["line 1"," line 2"]

>>> L.fold (L.groups lines (transduce newline) L.list) (map T.pack ["line 1\n line 2\n"])
["line 1","\n"," line 2","\n"]
-}
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

