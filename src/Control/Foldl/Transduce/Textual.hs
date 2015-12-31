{-# LANGUAGE ViewPatterns #-}

-- |
--
-- This module has transducers that work on 'Text' and other text-like types.
module Control.Foldl.Transduce.Textual {-# DEPRECATED "Use Control.Foldl.Transduce.Text instead." #-} (
        -- * Splitters
        textualSplit
    ,   textualBreak
        -- * Deprecated
    ,   textualSplitWhen
    ) where

import Data.Monoid (mempty)
import qualified Data.Monoid.Textual as MT
import qualified Data.Monoid.Null as MN
import Control.Foldl.Transduce


{- $setup

>>> import Control.Applicative
>>> import qualified Control.Foldl as L
>>> import Control.Foldl.Transduce

-}


{-| 

>>> L.fold (folds (textualSplit (=='.')) L.list L.list) [".","bb.bb","c.c."]
[[""],["","bb"],["bb","c"],["c"],[""]]
-}
textualSplit :: MT.TextualMonoid m => (Char -> Bool) -> Transducer m m ()
textualSplit predicate = Transducer step () done 
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
textualBreak :: MT.TextualMonoid m => (Char -> Bool) -> Transducer m m ()
textualBreak predicate = 
    Transducer step SplitWhenConditionPending done 
    where
        step SplitWhenConditionPending (MT.break (const False) predicate -> (i0,i1)) = 
            if MN.null i1
               then (SplitWhenConditionPending,[i0],[])
               else (SplitWhenConditionEncountered,[i0],[[i1]])
        step SplitWhenConditionEncountered i = 
               (SplitWhenConditionEncountered,[i],[])
        done = mempty

{-# DEPRECATED textualSplitWhen "use textualBreak instead" #-}
textualSplitWhen :: MT.TextualMonoid m => (Char -> Bool) -> Transducer m m ()
textualSplitWhen = textualBreak 

