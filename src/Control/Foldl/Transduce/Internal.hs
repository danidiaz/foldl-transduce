module Control.Foldl.Transduce.Internal (
        -- * Strict datatypes 
        Pair(..)
    ,   Trio(..)
    ) where

data Pair a b = Pair !a !b

data Trio a b c = Trio !a !b !c

