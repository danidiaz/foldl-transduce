module Control.Foldl.Transduce.Internal (
        -- * Strict datatypes 
        Pair(..)
    ,   Trio(..)
    ,   _1of3
    ) where

data Pair a b = Pair !a !b

data Trio a b c = Trio !a !b !c

_1of3 :: (a,b,c) -> a
_1of3 (x,_,_) = x
