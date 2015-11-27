{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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
    ,   words
    ,   lines
    ,   paragraphs
        -- * Re-exports
        -- $reexports
    ,   module Control.Foldl.Transduce.Textual
    ) where

import Prelude hiding (lines,words)
import Data.Char
import Data.Monoid (mempty)
import Data.Foldable (foldMap,foldl')
import qualified Data.ByteString as B
import qualified Data.Text 
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Exception.Base 
import qualified Control.Foldl.Transduce as L
import Control.Foldl.Transduce.Textual
import Control.Foldl.Transduce.Internal (Pair(..))
import qualified Data.List
import Data.List.Split
import qualified Data.List.Split
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

{- $setup

>>> :set -XFlexibleContexts
>>> import Data.String hiding (lines,words)
>>> import Data.Text (Text)
>>> import Control.Applicative
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
        (Pair leftovers next',[txt],[])
    done (Pair leftovers _) = 
        if B.null leftovers
            then ((), [], [])
            else ((), foldMap (pure . T.singleton) onLeftovers',[])
    onLeftovers' = onLeftovers "leftovers" Nothing

{-| Builds a UTF8-decoding 'Transducer'. Takes an error handler from
    "Data.Text.Encoding.Error".		

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
decoderE next = L.TransducerM step (return (Pair mempty next')) done
    where
        step (Pair _ next1) i = do
            emc <- liftIO . try . evaluate $ next1 i 
            case emc of 
                Left ue -> do
                    throwE ue
                Right (T.Some txt leftovers next2) -> do
                    return (Pair leftovers next2,[txt],[])
        done (Pair leftovers _) = do
            if B.null leftovers
                then return ((), [], [])
                else do
                    emc <- liftIO . try . evaluate $ onLeftovers'
                    case emc of
                        Left ue -> do
                            throwE ue
                        Right mc -> do
                            return ((), foldMap (return . T.singleton) mc,[])
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
        step True i = (True, [i],[])
        step False i =
            if blank i 
                then (False,[],[])
                else (True, [T.stripStart i],[])
        done _  = ((),[],[])

{-| Remove trailing white space from a stream of 'Text'.		

    __/BEWARE!/__ 
    This function naively accumulates in memory any arriving "blank blocks" of
    text until a non-blank block or end-of-stream arrives, and therefore it is
    potentially dangerous. Do not use with untrusted inputs.

>>> L.fold (transduce stripEnd L.list) (map T.pack [" ", " \n  text ", "   ", "" , " "])
[" "," \n  text"]
-}
stripEnd :: L.Transducer T.Text T.Text ()
stripEnd = L.Transducer step [] done
    where
        step txts i =
            if blank i
                -- dangerous!
                then (i:txts, [], [])
                else ([i], reverse txts, [])
        done txts = case reverse txts of
            txt : _ -> ((), [T.stripEnd txt], [])
            _ -> ((), [], [])

{-| Splits a stream of text into lines, removing the newlines.

>>> L.fold (L.groups lines (surround [T.pack "x"] []) L.list) (map T.pack ["line 1\n line 2\n"])
["x","line 1","x"," line 2"]

>>> L.fold (L.groups lines newline L.list) (map T.pack ["line 1\n line 2\n"])
["line 1","\n"," line 2","\n"]

    Used with 'L.transduce', it simply removes newlines:

>>> L.fold (L.transduce lines L.list) (map T.pack ["line 1\n line 2\n"])
["line 1"," line 2"]
-}
lines :: L.Transducer T.Text T.Text ()
lines = L.Transducer step False done 
    where
        step previousnl txt =
            if Data.Text.null txt
               then  
                   (previousnl,[],[])
               else
                   let
                       lastc = Data.Text.last txt == '\n'
                       txts = T.lines txt
                   in
                   case (previousnl,txts) of
                       (_,[]) -> error "never happens"
                       (True,_) -> (lastc, [], map pure txts)
                       (False,t:ts) -> (lastc, [t], map pure ts)

        done _ = ((),[],[])


data WordsState = 
      NoLastChar
    | LastCharSpace
    | LastCharNotSpace

{-| Splits a stream of text into words, removing whitespace.

>>> L.fold (folds words L.list L.list) (map T.pack ["  a","aa ", "bb c","cc dd ","ee f","f"])
[["a","aa"],["bb"],["c","cc"],["dd"],["ee"],["f","f"]]

    Used with 'L.transduce', it simply removes all whitespace:

>>> L.fold (L.transduce words L.list) (map T.pack ["  a","aa ", "bb c","cc dd ","ee f","f"])
["a","aa","bb","c","cc","dd","ee","f","f"]
-}
words :: L.Transducer T.Text T.Text ()
words = L.Transducer step NoLastChar done 
    where
        step tstate txt 
            | Data.Text.null txt = (tstate,[],[])
            | blank txt = 
                case tstate of
                    NoLastChar -> (NoLastChar,[],[])
                    _ -> (LastCharSpace,[],[])
            | otherwise =                    
                let nextstate = 
                        if isSpace (T.last txt) 
                           then LastCharSpace 
                           else LastCharNotSpace
                    (oldgroup,newgroups) = case (tstate, T.words txt) of
                        (NoLastChar,w:ws) -> 
                            ([w],map pure ws)
                        (LastCharSpace,ws) -> 
                            ([],map pure ws)
                        (LastCharNotSpace,w:ws) -> 
                            if isSpace (T.head txt)
                               then ([],map pure (w:ws))
                               else ([w],map pure ws)
                        (_,[]) -> error "never happens, txt not blank"
                in (nextstate,oldgroup,newgroups)
        done _ = ((),[],[])


data ParagraphsState = 
      SkippingAfterStreamStart
    | SkippingAfterNewline
    | SkippingAfterBlankLine
    | ContinuingNonemptyLine

{-| Splits a stream of text into paragraphs, removing empty lines and trimming
    newspace from the start of each line.

    Used with 'L.transduce', it removes empty lines and trims newspace from the
    start of each line.
-}
paragraphs :: L.Transducer T.Text T.Text ()
paragraphs = L.Transducer step SkippingAfterStreamStart done 
    where
        step tstate txt
            | Data.Text.null txt = 
                (tstate,[],[])
            | otherwise = 
                let (initlines,lastline) = splittedLines txt
                    (tstate', outputsreversed) =
                        advanceLast
                        (foldl' 
                            advance
                            (tstate,pure [])
                            initlines)
                        lastline          
                    (xs :| xss) = fmap reverse (NonEmpty.reverse outputsreversed)
                in (tstate',xs,xss)
        done _ = 
            ((),[],[])
        splittedLines :: T.Text -> ([T.Text],T.Text)
        splittedLines nonEmptyChunk = 
            let splitted = 
                    Data.Text.lines nonEmptyChunk 
                    ++
                    if T.last nonEmptyChunk == '\n' then [mempty] else mempty
            in (init splitted, last splitted) -- unsafe with empty lists!!!
        advance :: (ParagraphsState, NonEmpty [T.Text]) -> T.Text -> (ParagraphsState, NonEmpty [T.Text])
        advance (s,outputs) i = 
            case (s, blank i) of
                (SkippingAfterStreamStart, True) -> (SkippingAfterStreamStart,outputs)
                (SkippingAfterStreamStart, False) -> (SkippingAfterNewline,prepend ["\n",T.stripStart i] outputs) 
                (SkippingAfterNewline, True) -> (SkippingAfterBlankLine, outputs) 
                (SkippingAfterNewline, False) -> (SkippingAfterNewline,prepend ["\n",T.stripStart i] outputs) 
                (SkippingAfterBlankLine, True) -> (SkippingAfterBlankLine,outputs) 
                (SkippingAfterBlankLine, False) -> (SkippingAfterNewline, prepend ["\n",T.stripStart i] (NonEmpty.cons [] outputs)) 
                (ContinuingNonemptyLine, _) -> (SkippingAfterNewline,prepend ["\n",i] outputs)
        advanceLast :: (ParagraphsState, NonEmpty [T.Text]) -> T.Text -> (ParagraphsState, NonEmpty [T.Text])
        advanceLast (s,outputs) i = 
            case (s, blank i) of
                (SkippingAfterStreamStart, True) -> (SkippingAfterStreamStart,outputs)
                (SkippingAfterStreamStart, False) -> (ContinuingNonemptyLine,prepend [T.stripStart i] outputs)
                (SkippingAfterNewline, True) -> (SkippingAfterNewline,outputs)
                (SkippingAfterNewline, False) -> (ContinuingNonemptyLine,prepend [T.stripStart i] outputs)
                (SkippingAfterBlankLine, True) -> (SkippingAfterBlankLine,outputs) 
                (SkippingAfterBlankLine, False) -> (ContinuingNonemptyLine,prepend [T.stripStart i] (NonEmpty.cons [] outputs))
                (ContinuingNonemptyLine, _) -> (ContinuingNonemptyLine,prepend [i] outputs)
        prepend :: [a] -> NonEmpty [a] -> NonEmpty [a]
        prepend as (as':| rest) = (as ++ as') :| rest

------------------------------------------------------------------------------

{- $reexports

-}
