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
    ,   sections
        -- * Textual
        -- $textual
    ,   textualSplit
    ,   textualBreak
    ) where

import Prelude hiding (lines,words)
import Data.Char
import Data.Bool
import Data.Maybe
import Data.List (unfoldr)
import Data.Monoid (mempty,(<>))
import Data.Foldable (foldMap,foldl')
import qualified Data.ByteString as B
import qualified Data.Text 
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Monoid.Textual as MT
import qualified Data.Monoid.Null as MN
import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Exception.Base 
import qualified Control.Foldl.Transduce as L
import qualified Data.List
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

data Pair a b = Pair !a !b

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

>>> map mconcat (L.fold (folds paragraphs L.list L.list) (map T.pack [" \n aaa","\naa ", " \n\nbb\n"]))
["aaa\naa  \n","bb\n"]

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
        advance 
            :: (ParagraphsState, NonEmpty [T.Text]) 
            -> T.Text 
            -> (ParagraphsState, NonEmpty [T.Text])
        advance (s,outputs) i = 
            case (s, blank i) of
                (SkippingAfterStreamStart, True) -> 
                    (,) 
                    SkippingAfterStreamStart 
                    outputs
                (SkippingAfterStreamStart, False) -> 
                    (,)
                    SkippingAfterNewline
                    (continue ["\n",T.stripStart i] outputs) 
                (SkippingAfterNewline, True) -> 
                    (,) 
                    SkippingAfterBlankLine 
                    outputs 
                (SkippingAfterNewline, False) -> 
                    (,)
                    SkippingAfterNewline
                    (continue ["\n",T.stripStart i] outputs)
                (SkippingAfterBlankLine, True) -> 
                    (,) 
                    SkippingAfterBlankLine 
                    outputs 
                (SkippingAfterBlankLine, False) -> 
                    (,)
                    SkippingAfterNewline
                    (continue ["\n",T.stripStart i] (NonEmpty.cons [] outputs)) 
                (ContinuingNonemptyLine, _) -> 
                    (,)
                    SkippingAfterNewline
                    (continue ["\n",i] outputs)
        advanceLast 
                :: (ParagraphsState, NonEmpty [T.Text]) 
                -> T.Text 
                -> (ParagraphsState, NonEmpty [T.Text])
        advanceLast (s,outputs) i = 
            case (s, blank i) of
                (SkippingAfterStreamStart, True) -> 
                    (,) 
                    SkippingAfterStreamStart 
                    outputs
                (SkippingAfterStreamStart, False) -> 
                    (,)
                    ContinuingNonemptyLine
                    (continue [T.stripStart i] outputs)
                (SkippingAfterNewline, True) -> 
                    (,) 
                    SkippingAfterNewline 
                    outputs
                (SkippingAfterNewline, False) -> 
                    (,)
                    ContinuingNonemptyLine
                    (continue [T.stripStart i] outputs)
                (SkippingAfterBlankLine, True) -> 
                    (,)
                    SkippingAfterBlankLine
                    outputs 
                (SkippingAfterBlankLine, False) -> 
                    (,)
                    ContinuingNonemptyLine
                    (continue [T.stripStart i] (NonEmpty.cons [] outputs))
                (ContinuingNonemptyLine, _) -> 
                    (,)
                    ContinuingNonemptyLine
                    (continue [i] outputs)

{-| 

    Given a (possibly infinite) list of section headings, split the stream into
    sections and remove the headings. 

>>> map mconcat (L.fold (folds (sections (map T.pack ["#1\n","#2\n"])) L.list L.list) (map T.pack [" #1\naa\n#","2\nbb"]))
[" ","aa\n","bb"]

>>> map mconcat (L.fold (folds (sections (map T.pack ["1234"])) L.list L.list) (map T.pack [" 1","2","x","1","2","3","4","5"]))
[" 12x","5"]

    Used with 'L.transduce', it simply removes all headings.
-}
sections :: [T.Text] -> L.Transducer T.Text T.Text ()
sections seps = L.Transducer step (initialstate seps) done 
    where
        step tstate txt =
            let (emitted,fmap snd -> states) = Data.List.unzip (unfoldWithState splitTextStep (txt,tstate))
                finalState = NonEmpty.last (tstate :| states)
                continuing :| following = NonEmpty.reverse (fmap Data.List.reverse (foldl' advance ([]:|[]) emitted))
            in (finalState, continuing, following)                        
        advance :: NonEmpty [x] -> ([x],Bool) -> NonEmpty [x]
        advance l (e,b) = bool id (separate []) b (continue e l)
        done Done = 
            ((),[],[])
        done (Pending acc _ _) =
            ((),[acc],[])    
        initialstate [] = Done
        initialstate (x:xs) = Pending T.empty x xs


continue :: [a] -> NonEmpty [a] -> NonEmpty [a]
continue as (as':| rest) = (as ++ as') :| rest

separate :: [x] -> NonEmpty [x] -> NonEmpty [x]
separate = NonEmpty.cons

data SectionsState = 
      Done
    | Pending T.Text T.Text [T.Text] -- first is the accumulator
    deriving (Show)

{-| 		

>>> splitTextStep (T.pack "x",Done)
Just ((["x"],False),("",Done))

>>> splitTextStep (T.pack "aabbcc",Pending T.empty (T.pack "bb") [])
Just ((["aa"],True),("cc",Done))

>>> splitTextStep (T.pack "cc",Pending (T.pack "bb") (T.pack "bbcc") [T.pack "nextsep"])
Just (([""],True),("",Pending "" "nextsep" []))

>>> splitTextStep (T.pack "xx",Pending (T.pack "bb") (T.pack "bbcc") [])
Just ((["bbxx"],False),("",Pending "" "bbcc" []))

>>> splitTextStep (T.pack "xbb",Pending (T.pack "bbc") (T.pack "bbcccc") [])
Just ((["bbcx"],False),("",Pending "bb" "bbcccc" []))

-}
splitTextStep 
    :: (T.Text, SectionsState) 
    -> Maybe (([T.Text],Bool), (T.Text, SectionsState))
splitTextStep (txt, _) | T.null txt           = Nothing
splitTextStep (txt, Done)                     = Just (([txt],False),(T.empty,Done))
splitTextStep (txt, Pending acc sep nextseps) = Just $
    let (before,after) = T.breakOn sep (acc <> txt)
    in
    if T.null after 
       then -- not present
          let (m0,m) = maxintersect before sep
          in
          (([m0],False),(T.empty, Pending m sep nextseps))
       else -- present
          let unprefixed = T.drop (T.length sep) after
              nextstate = case nextseps of
                  [] -> Done
                  z:zs -> Pending T.empty z zs
          in
          (([before],True),(unprefixed,nextstate))
                               
maxintersect :: T.Text -> T.Text -> (T.Text,T.Text)
maxintersect txt sep = 
    let prefixes = (tail . reverse . tail . T.inits) sep 
        partialmatches = filter (flip T.isSuffixOf txt) prefixes
        m = maybe T.empty id (listToMaybe partialmatches)
    in
    (T.take (T.length txt - T.length m) txt,m)
        
unfoldWithState :: (b -> Maybe (a, b)) -> b -> [(a, b)]
unfoldWithState f = unfoldr (fmap (\t@(_, b) -> (t, b)) . f)

------------------------------------------------------------------------------

{- $textual

    Transducers that work on 'Text' and other text-like types.

-}

{-| 

>>> L.fold (folds (textualSplit (=='.')) L.list L.list) [".","bb.bb","c.c."]
[[""],["","bb"],["bb","c"],["c"],[""]]

-}

textualSplit :: MT.TextualMonoid m => (Char -> Bool) -> L.Transducer m m ()
textualSplit predicate = L.Transducer step () done 
  where
    step _ txt = case MT.split predicate txt of
        x:xs -> ((),[x],map (:[]) xs)
        _ -> error "never happens"
    done _ = mempty


data SplitWhenWhenState = 
      SplitWhenConditionEncountered 
    | SplitWhenConditionPending

{-| 		

>>> L.fold (bisect (textualBreak (=='.')) (reify id) ignore L.list) ["aa","bb.bb","cc"]
["aa","bb"]
-}
textualBreak :: MT.TextualMonoid m => (Char -> Bool) -> L.Transducer m m ()
textualBreak predicate = 
    L.Transducer step SplitWhenConditionPending done 
    where
        step SplitWhenConditionPending (MT.break (const False) predicate -> (i0,i1)) = 
            if MN.null i1
               then (SplitWhenConditionPending,[i0],[])
               else (SplitWhenConditionEncountered,[i0],[[i1]])
        step SplitWhenConditionEncountered i = 
               (SplitWhenConditionEncountered,[i],[])
        done = mempty
