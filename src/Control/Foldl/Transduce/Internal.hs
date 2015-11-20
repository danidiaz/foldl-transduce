module Control.Foldl.Transduce.Internal (
        -- * Strict datatypes 
        Pair(..)
    ,   Quartet(..)
    ,   fst3
    ) where

data Pair a b = Pair !a !b

data Quartet a b c d = Quartet !a !b !c !d

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
