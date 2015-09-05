module Control.Foldl.Transduce.Internal (
        -- * Strict datatypes 
        Pair(..)
    ,   Quartet(..)
    ,   _1of3
    ) where

data Pair a b = Pair !a !b

data Quartet a b c d = Quartet !a !b !c !d

_1of3 :: (a,b,c) -> a
_1of3 (x,_,_) = x
