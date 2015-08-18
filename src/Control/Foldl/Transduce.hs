{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Foldl.Transduce (
        Transduction 
    ,   TransductionM
    ,   Transducer(..)
    ,   TransducerM(..)
    ,   with
    ,   with'
    ,   withM
    ,   withM'
    ,   generalize'
    ,   simplify'
    ,   foldify
    ,   foldifyM
    ,   prefixSuffix
    ,   withG
    ,   module Control.Foldl
    ) where

import Data.Bifunctor
import Data.Functor.Identity
import Data.Foldable (foldrM,toList)
import Control.Monad
import Control.Comonad
import Control.Foldl (Fold(..),FoldM(..))
import qualified Control.Foldl as L
import Control.Foldl.Transduce.Internal(Pair(..))


instance Comonad (Fold a) where
    extract (Fold _ begin done) = done begin
    {-#  INLINABLE extract #-}

    duplicate (Fold step begin done) = Fold step begin (\x -> Fold step x done)
    {-#  INLINABLE duplicate #-}

------------------------------------------------------------------------------

type Transduction a b = forall x. Fold b x -> Fold a x

type TransductionM m a b = forall x. Monad m => FoldM m b x -> FoldM m a x

data Transducer i o r
     = forall x. Transducer (x -> i -> (x,[o])) x (x -> (r,[o]))

instance Functor (Transducer i o) where
    fmap f (Transducer step begin done) = Transducer step begin (first f . done)

instance Bifunctor (Transducer i) where
    first f (Transducer step begin done) =
        Transducer (fmap (fmap (fmap f)) . step) begin (fmap (fmap f) . done)
    second f w = fmap f w

data TransducerM m i o r
     = forall x. TransducerM (x -> i -> m (x,[o])) (m x) (x -> m (r,[o]))

instance Functor m => Functor (TransducerM m i o) where
    fmap f (TransducerM step begin done) = TransducerM step begin (fmap (first f) . done)

instance Functor m => Bifunctor (TransducerM m i) where
    first f (TransducerM step begin done) =
        TransducerM (fmap (fmap (fmap (fmap f))) . step) begin (fmap (fmap (fmap f)) . done)
    second f w = fmap f w

with :: Transducer i o r -> Transduction i o 
with = with' (flip const) 

with' :: (x -> y -> z) -> Transducer i o x -> Fold o y -> Fold i z
with' f (Transducer wstep wstate wdone) (Fold fstep fstate fdone) =
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


withM :: Monad m => TransducerM m i o r -> TransductionM m i o 
withM = withM' (flip const)

withM' :: Monad m => (x -> y -> z) -> TransducerM m i o x -> FoldM m o y -> FoldM m i z
withM' f (TransducerM wstep wstate wdone) (FoldM fstep fstate fdone) =
    FoldM step (liftM2 Pair wstate fstate) done 
        where
            step (Pair ws fs) i = do
                (ws',os) <- wstep ws i
                liftM (Pair ws') (foldrM (flip fstep) fs os)
            done (Pair ws fs) = do
                (wr,os) <- wdone ws
                liftM (f wr) (fdone =<< foldrM (flip fstep) fs os)


generalize' :: Monad m => Transducer i o r -> TransducerM m i o r
generalize' (Transducer step begin done) = TransducerM step' begin' done'
  where
    step' x a = return (step x a)
    begin'    = return  begin
    done' x   = return (done x)

simplify' :: TransducerM Identity i o r -> Transducer i o r
simplify' (TransducerM step begin done) = Transducer step' begin' done' where
    step' x a = runIdentity (step x a)
    begin'    = runIdentity  begin
    done' x   = runIdentity (done x)

foldify :: Transducer i o r -> Fold i r
foldify (Transducer step begin done) =
    Fold (\x i -> fst (step x i)) begin (\x -> fst (done x))

foldifyM :: Functor m => TransducerM m i o r -> FoldM m i r
foldifyM (TransducerM step begin done) =
    FoldM (\x i -> fmap fst (step x i)) begin (\x -> fmap fst (done x))

------------------------------------------------------------------------------

data PrefixSuffixState = PrefixAdded | PrefixPending

--  L.fold (with (prefixSuffix "ab" "cd") L.list) "xy"
--  L.fold (with (prefixSuffix "ab" "cd") L.list) ""
prefixSuffix :: (Foldable p, Foldable s) => p a -> s a -> Transducer a a ()
prefixSuffix (reverse . toList -> ps) (reverse . toList -> ss) = 
    Transducer step PrefixPending done 
    where
        step PrefixPending a = 
            (PrefixAdded, [a] ++ ps)
        step PrefixAdded a = 
            (PrefixAdded, [a])
        done PrefixPending = ((), ss ++ ps)
        done PrefixAdded = ((), ss)

------------------------------------------------------------------------------

data Snoc i = Snoc (Maybe (Snoc i)) i

foldsnoc :: (b -> b) -> (a -> b -> b) -> b -> Snoc a -> b
foldsnoc g f b sn = go sn b  
    where
        go (Snoc Nothing a) b' = f a b'
        go (Snoc (Just sn') a) b' = go sn' (g (f a b'))

data Splitter i
     = forall x. Splitter (x -> i -> (x,Snoc [i])) x (x -> [i])

withG :: Splitter i -> Transduction i b -> Transduction i b 
withG (Splitter sstep sbegin sdone) t f =
    let 
        step (Pair ss fs) i = 
           let (ss', sn) = sstep ss i
           in
           Pair ss' (foldsnoc reset step' fs sn)  
        step' is (Fold fstep fstate fdone) =
           Fold fstep (foldr (flip fstep) fstate is) fdone  
        reset (Fold _ fstate fdone) = 
           t (duplicate (fdone fstate)) 
        done (Pair ss (Fold fstep fstate fdone)) = 
            extract (fdone (foldr (flip fstep) fstate (sdone ss)))
    in 
    Fold step (Pair sbegin (t (duplicate f))) done 

