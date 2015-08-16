{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Foldl.Transduce (
        Transducer 
    ,   TransducerM
    ,   Wrap(..)
    ,   WrapM(..)
    ,   transduce
    ,   transduce'
    ,   transduceM
    ,   transduceM'
    ,   generalize'
    ,   simplify'
    ,   foldify
    ,   foldifyM
    ,   prefixSuffix
    ,   module Control.Foldl
    ) where

import Data.Bifunctor
import Data.Functor.Identity
import Data.Foldable (foldrM,toList)
import Control.Monad
import Control.Comonad
import Control.Foldl (Fold(..),FoldM(..))
import qualified Control.Foldl as L


instance Comonad (Fold a) where
    extract (Fold _ begin done) = done begin
    {-#  INLINABLE extract #-}

    duplicate (Fold step begin done) = Fold step begin (\x -> Fold step x done)
    {-#  INLINABLE duplicate #-}

data Pair a b = Pair !a !b

------------------------------------------------------------------------------

type Transducer a b = forall x. Fold b x -> Fold a x

type TransducerM m a b = forall x. Monad m => FoldM m b x -> FoldM m a x

data Wrap i o r
     = forall x. Wrap (x -> i -> (x,[o])) x (x -> (r,[o]))

instance Functor (Wrap i o) where
    fmap f (Wrap step begin done) = Wrap step begin (first f . done)

instance Bifunctor (Wrap i) where
    first f (Wrap step begin done) =
        Wrap (fmap (fmap (fmap f)) . step) begin (fmap (fmap f) . done)
    second f w = fmap f w

data WrapM m i o r
     = forall x. WrapM (x -> i -> m (x,[o])) (m x) (x -> m (r,[o]))

instance Functor m => Functor (WrapM m i o) where
    fmap f (WrapM step begin done) = WrapM step begin (fmap (first f) . done)

instance Functor m => Bifunctor (WrapM m i) where
    first f (WrapM step begin done) =
        WrapM (fmap (fmap (fmap (fmap f))) . step) begin (fmap (fmap (fmap f)) . done)
    second f w = fmap f w

transduce :: Wrap i o r -> Transducer i o 
transduce = transduce' (flip const) 

transduce' :: (x -> y -> z) -> Wrap i o x -> Fold o y -> Fold i z
transduce' f (Wrap wstep wstate wdone) (Fold fstep fstate fdone) =
    Fold step (Pair wstate fstate) done 
        where
            step (Pair ws fs) i = 
                let (ws',os) = wstep ws i 
                in
                Pair ws' (foldr (flip fstep) fs os)  
            done (Pair ws fs) = 
                let (wr,os) = wdone ws
                in 
                f wr (fdone (foldr (flip fstep) fs os))


transduceM :: Monad m => WrapM m i o r -> TransducerM m i o 
transduceM = transduceM' (flip const)

transduceM' :: Monad m => (x -> y -> z) -> WrapM m i o x -> FoldM m o y -> FoldM m i z
transduceM' f (WrapM wstep wstate wdone) (FoldM fstep fstate fdone) =
    FoldM step (liftM2 Pair wstate fstate) done 
        where
            step (Pair ws fs) i = do
                (ws',os) <- wstep ws i
                liftM (Pair ws') (foldrM (flip fstep) fs os)
            done (Pair ws fs) = do
                (wr,os) <- wdone ws
                liftM (f wr) (fdone =<< foldrM (flip fstep) fs os)


generalize' :: Monad m => Wrap i o r -> WrapM m i o r
generalize' (Wrap step begin done) = WrapM step' begin' done'
  where
    step' x a = return (step x a)
    begin'    = return  begin
    done' x   = return (done x)

simplify' :: WrapM Identity i o r -> Wrap i o r
simplify' (WrapM step begin done) = Wrap step' begin' done' where
    step' x a = runIdentity (step x a)
    begin'    = runIdentity  begin
    done' x   = runIdentity (done x)

foldify :: Wrap i o r -> Fold i r
foldify (Wrap step begin done) =
    Fold (\x i -> fst (step x i)) begin (\x -> fst (done x))

foldifyM :: Functor m => WrapM m i o r -> FoldM m i r
foldifyM (WrapM step begin done) =
    FoldM (\x i -> fmap fst (step x i)) begin (\x -> fmap fst (done x))

------------------------------------------------------------------------------

data PrefixSuffixState = PrefixAdded | PrefixPending

--  L.fold (transduce (prefixSuffix "ab" "cd") L.list) "xy"
--  L.fold (transduce (prefixSuffix "ab" "cd") L.list) ""
prefixSuffix :: (Foldable p, Foldable s) => p a -> s a -> Wrap a a ()
prefixSuffix (reverse . toList -> ps) (reverse . toList -> ss) = 
    Wrap step PrefixPending done 
    where
        step PrefixPending a = 
            (PrefixAdded, [a] ++ ps)
        step PrefixAdded a = 
            (PrefixAdded, [a])
        done PrefixPending = ((), ss ++ ps)
        done PrefixAdded = ((), ss)

------------------------------------------------------------------------------

-- When there's no maybe, no change of is required
data Snoc i = Snoc (Maybe (Snoc i)) i

-- totally reinventing the wheel here
foldrsnoc :: (b -> b) -> (a -> b -> b) -> b -> Snoc a -> b
foldrsnoc g f b sn = go sn b  
    where
        go (Snoc Nothing a) b' = f a b'
        go (Snoc (Just sn') a) b' = go sn' (g (f a b'))

data Splitter i
     = forall x. Splitter (x -> i -> (x,Snoc [i])) x (x -> [i])

-- You can pass "prefix" as the transducer, for example...
-- pregroup :: Splitter i -> Transducer i b -> Transducer i b 
-- pregroup (Splitter sstep sbegin sdone) t f =
--     case t (duplicate f) of 
--         Fold fstep fbegin fdone ->
--             let 
--                 reset fs = case t (duplicate (fdone fs)) of 
--                     Fold _ fbegin' _ -> fbegin'
--                 step (Pair ss fs) i = 
--                     let (ss', sn) = sstep ss i
--                     in
--                     -- still needs work
--                     Pair ss' (foldrsnoc reset (\is sn' -> foldr (flip fstep) sn' is) fs sn)  
--                 done (Pair ss fs) = 
--                     extract (fdone (foldr (flip fstep) fs (sdone ss)))
--             in 
--             Fold step (Pair sbegin fbegin) done 

-- condense?

--chunksOf :: Int -> Transducer a [a]
--chunksOf groupSize = transduce $ 
--    Wrap step (Pair 0 []) done 
--    where
--        step (Pair i as) a = 
--            let i' = succ i 
--                as' = a:as
--            in
--            if (i' == groupSize)
--               then (Pair 0 [], [reverse as'])
--               else (Pair i' as', [])
--
--        done (Pair _ []) = ((), [])
--        done (Pair _ as) = ((), [reverse as])
