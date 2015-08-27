module Control.Foldl.Transduce.Internal (
        -- * Strict datatypes 
        Pair(..)
    ,   Trio(..)
    ,   fstOf3
    ) where

data Pair a b = Pair !a !b

data Trio a b c = Trio !a !b !c

fstOf3 :: (a,b,c) -> a
fstOf3 (x,_,_) = x
