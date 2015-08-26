module Control.Foldl.Transduce.Internal (
        -- * Strict datatypes 
        Pair(..)
    ,   Trio(..)
--    ,   Quartet(..)
    ) where

data Pair a b = Pair !a !b

data Trio a b c = Trio !a !b !c

--data Quartet a b c d  = Quartet !a !b !c !d

