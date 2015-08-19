{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Foldl.Transduce (
        -- * Transducer types
        Transduction 
    ,   Transducer(..)
    ,   TransductionM
    ,   TransducerM(..)
        -- * Applying transducers
    ,   with
    ,   with'
    ,   withM
    ,   withM'
        -- * Transducers
    ,   surround
    ,   surroundIO
        -- * Transducer utilities
    ,   generalize'
    ,   simplify'
    ,   foldify
    ,   foldifyM
    ,   chokepoint 
    ,   chokepointM
    ,   duplicateM
        -- * Splitter types
    ,   Splitter(..)
        -- * Working with groups
    ,   withG
    ,   withGM
    ,   foldG
        -- * Splitters
    ,   chunksOf
        -- * Re-exports
        -- $reexports
    ,   module Control.Foldl
    ) where

import Data.Bifunctor
import Data.Functor.Identity
import Data.Foldable (foldlM,foldl',toList)
import Control.Monad
import Control.Monad.IO.Class
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


data Transducer i o r
     = forall x. Transducer (x -> i -> (x,[o])) x (x -> (r,[o]))

instance Functor (Transducer i o) where
    fmap f (Transducer step begin done) = Transducer step begin (first f . done)

instance Bifunctor (Transducer i) where
    first f (Transducer step begin done) =
        Transducer (fmap (fmap (fmap f)) . step) begin (fmap (fmap f) . done)
    second f w = fmap f w

type TransductionM m a b = forall x. Monad m => FoldM m b x -> FoldM m a x

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
                Pair ws' (foldl' fstep fs os)  
            done (Pair ws fs) = 
                let (wr,os) = wdone ws
                in 
                f wr (fdone (foldl' fstep fs os))


withM :: Monad m => TransducerM m i o r -> TransductionM m i o 
withM = withM' (flip const)

withM' :: Monad m => (x -> y -> z) -> TransducerM m i o x -> FoldM m o y -> FoldM m i z
withM' f (TransducerM wstep wstate wdone) (FoldM fstep fstate fdone) =
    FoldM step (liftM2 Pair wstate fstate) done 
        where
            step (Pair ws fs) i = do
                (ws',os) <- wstep ws i
                liftM (Pair ws') (foldlM fstep fs os)
            done (Pair ws fs) = do
                (wr,os) <- wdone ws
                liftM (f wr) (fdone =<< foldlM fstep fs os)

------------------------------------------------------------------------------

data SurroundState = PrefixAdded | PrefixPending

surround :: (Foldable p, Foldable s) => p a -> s a -> Transducer a a ()
surround (toList -> ps) (toList -> ss) = 
    Transducer step PrefixPending done 
    where
        step PrefixPending a = 
            (PrefixAdded, ps ++ [a])
        step PrefixAdded a = 
            (PrefixAdded, [a])
        done PrefixPending = ((), ps ++ ss)
        done PrefixAdded = ((), ss)

surroundIO :: (Foldable p, Foldable s, MonadIO m) 
           => m (p a) 
           -> m (s a) 
           -> TransducerM m a a ()
surroundIO prefixa suffixa = 
    TransducerM step (return PrefixPending) done 
    where
        step PrefixPending a = do
            ps <- fmap toList prefixa
            return (PrefixAdded, ps ++ [a])
        step PrefixAdded a = 
            return (PrefixAdded, [a])
        done PrefixPending = do
            ps <- fmap toList prefixa
            ss <- fmap toList suffixa
            return ((), toList ps ++ toList ss)
        done PrefixAdded = do
            ss <- fmap toList suffixa
            return ((), toList ss)

------------------------------------------------------------------------------

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

chokepoint :: Fold i b -> Transducer i b ()
chokepoint (Fold fstep fstate fdone) =
    (Transducer wstep fstate wdone)
    where
        wstep = \fstate' i -> (fstep fstate' i,[])
        wdone = \fstate' -> ((),[fdone fstate'])

chokepointM :: Applicative m => FoldM m i b -> TransducerM m i b ()
chokepointM (FoldM fstep fstate fdone) = 
    (TransducerM wstep fstate wdone)
    where
        wstep = \fstate' i -> fmap (\s -> (s,[])) (fstep fstate' i)
        wdone = \fstate' -> fmap (\r -> ((),[r])) (fdone fstate')

duplicateM :: Applicative m => FoldM m a b -> FoldM m a (FoldM m a b)
duplicateM (FoldM step begin done) = 
    FoldM step begin (\x -> pure (FoldM step (pure x) done))
{-#  INLINABLE duplicateM #-}

------------------------------------------------------------------------------

data Splitter i
     = forall x. Splitter (x -> i -> (x,[i],[[i]])) x (x -> [i])

withG :: Splitter i -> Transduction i b -> Transduction i b 
withG (Splitter sstep sbegin sdone) t f =
    Fold step (Pair sbegin (t (duplicate f))) done 
    where
        step (Pair ss fs) i = 
           let 
               (ss', oldSplit, newSplits) = sstep ss i
               fs' = foldl' (step' . reset) (step' fs oldSplit) newSplits
           in
           Pair ss' fs'
        step' = L.fold . duplicate
        reset (Fold _ fstate fdone) = 
           t (duplicate (fdone fstate)) 
        done (Pair ss (Fold fstep fstate fdone)) = 
            extract (fdone (foldl' fstep fstate (sdone ss)))

withGM :: Monad m => Splitter i -> TransductionM m i b -> TransductionM m i b
withGM (Splitter sstep sbegin sdone) t f = 
    FoldM step (return (Pair sbegin (t (duplicateM f)))) done        
    where
        step (Pair ss fs) i = do
             let 
                 (ss', oldSplit, newSplits) = sstep ss i
             fs' <- step' fs oldSplit
             fs'' <- foldlM step'' fs' newSplits
             return (Pair ss' fs'')
        step' = L.foldM . duplicateM
        step'' = \fs is -> reset fs >>= \fs' -> step' fs' is
        reset (FoldM _ fstate fdone) = 
           liftM (t . duplicateM) (fstate >>= fdone) 
        done (Pair ss (FoldM fstep fstate fdone)) = do
            finalf <- fdone =<< flip (foldlM fstep) (sdone ss) =<< fstate
            L.foldM finalf [] 

foldG :: Splitter i -> Fold i b -> Transduction i b
foldG splitter f = withG splitter (with (chokepoint f))

------------------------------------------------------------------------------

chunksOf :: Int -> Splitter a
chunksOf groupSize = Splitter step 0 done 
    where
        step i a =
            let i' = succ i in
            if (i' == groupSize)
               then (0, [], [[a]])
               else (i',[a],[])
        done _ = []

------------------------------------------------------------------------------

{- $reexports

-}
