{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

-- |
--
-- This module builds on module "Control.Foldl", adding stateful transducers
-- and grouping operations.

module Control.Foldl.Transduce (
        -- * Transducer types
        Transduction 
    ,   Transducer(..)
    ,   TransductionM
    ,   TransducerM(..)
        -- * Applying transducers
    ,   transduce
    ,   transduce'
    ,   transduceM
    ,   transduceM'
        -- * Transducers
    ,   surround
    ,   surroundIO
        -- * Transducer utilities
    ,   generalizeTransducer
    ,   simplifyTransducer
    ,   foldify
    ,   foldifyM
    ,   chokepoint 
    ,   chokepointM
    ,   hoistTransducer
    ,   hoistFold
        -- * Splitter types
    ,   Splitter(..)
        -- * Working with groups
    ,   groups
    ,   groupsM
    ,   folds
    ,   foldsM
        -- * Splitters
    ,   chunksOf
        -- * Re-exports
        -- $reexports
    ,   module Data.Functor.Extend
    ,   module Control.Foldl
    ) where

import Data.Bifunctor
import Data.Functor.Identity
import Data.Functor.Extend
import Data.Foldable (foldlM,foldl',toList)
import Control.Monad
import Control.Monad.IO.Class
import Control.Comonad
import Control.Foldl (Fold(..),FoldM(..))
import qualified Control.Foldl as L
import Control.Foldl.Transduce.Internal(Pair(..))

{- $setup

>>> import qualified Control.Foldl as L
>>> import Control.Foldl.Transduce

-}

------------------------------------------------------------------------------

#if !(MIN_VERSION_foldl(1,1,2))
instance Comonad (Fold a) where
    extract (Fold _ begin done) = done begin
    {-# INLINABLE extract #-}

    duplicate (Fold step begin done) = Fold step begin (\x -> Fold step x done)
    {-# INLINABLE duplicate #-}
#endif

instance Extend (Fold a) where
    duplicated f = duplicate f
    {-# INLINABLE duplicated #-}

instance Monad m => Extend (FoldM m a) where
    duplicated (FoldM step begin done) = 
        FoldM step begin (\x -> pure $! FoldM step (pure x) done)
    {-# INLINABLE duplicated #-}

------------------------------------------------------------------------------

{-| A (possibly stateful) transformation on the inputs of a 'Fold'.

    Functions constructed with combinators like 'L.premap' of "Control.Foldl"
    also typecheck as 'Transduction'.
-}
type Transduction a b = forall x. Fold b x -> Fold a x


{-| Representation of a stateful 'Transduction' with step function, an initial
    accumulator, and a extraction function that returns a summary value of type
    @r@. Both the step function and the extraction function may send output
    downstream.

-}
data Transducer i o r
     = forall x. Transducer (x -> i -> (x,[o])) x (x -> (r,[o]))

instance Functor (Transducer i o) where
    fmap f (Transducer step begin done) = Transducer step begin (first f . done)

instance Bifunctor (Transducer i) where
    first f (Transducer step begin done) =
        Transducer (fmap (fmap (fmap f)) . step) begin (fmap (fmap f) . done)
    second f w = fmap f w

type TransductionM m a b = forall x. Monad m => FoldM m b x -> FoldM m a x

{-| Like 'Transducer', but monadic.

-}
data TransducerM m i o r
     = forall x. TransducerM (x -> i -> m (x,[o])) (m x) (x -> m (r,[o]))

instance Monad m => Functor (TransducerM m i o) where
    fmap f (TransducerM step begin done) = TransducerM step begin done'
      where
        done' x = do
            (r,os) <- done x
            let r' = f r
            return $! (r' `seq` (r', os))

instance Monad m => Bifunctor (TransducerM m i) where
    first f (TransducerM step begin done) =
        TransducerM (fmap (fmap (fmap (fmap f))) . step) begin (fmap (fmap (fmap f)) . done)
    second f w = fmap f w

{-| Apply a 'Transducer' to a 'Fold', discarding the return value of the
    'Transducer'.		

>>> L.fold (transduce (Transducer (\_ i -> ((),[i])) () (\_ -> ('r',[]))) L.list) [1..7]
[1,2,3,4,5,6,7]
-}
transduce :: Transducer i o r -> Transduction i o 
transduce t = fmap snd . (transduce' t)

{-| Generalized version of 'transduce' that preserves the return value of
    the 'Transducer'.

>>> L.fold (transduce' (Transducer (\_ i -> ((),[i])) () (\_ -> ('r',[]))) L.list) [1..7]
('r',[1,2,3,4,5,6,7])
-}
transduce' :: Transducer i o x -> Fold o y -> Fold i (x,y)
transduce' (Transducer wstep wstate wdone) (Fold fstep fstate fdone) =
    Fold step (Pair wstate fstate) done 
        where
            step (Pair ws fs) i = 
                let (ws',os) = wstep ws i 
                in
                Pair ws' (foldl' fstep fs os)  
            done (Pair ws fs) = 
                let (wr,os) = wdone ws
                in 
                (,) wr (fdone (foldl' fstep fs os))


transduceM :: Monad m => TransducerM m i o r -> TransductionM m i o 
transduceM t = fmap snd . (transduceM' t)

transduceM' :: Monad m => TransducerM m i o x -> FoldM m o y -> FoldM m i (x,y)
transduceM' (TransducerM wstep wstate wdone) (FoldM fstep fstate fdone) =
    FoldM step (liftM2 Pair wstate fstate) done 
        where
            step (Pair ws fs) i = do
                (ws',os) <- wstep ws i
                fs' <- foldlM fstep fs os
                return $! Pair ws' fs'
            done (Pair ws fs) = do
                (wr,os) <- wdone ws
                fr <- fdone =<< foldlM fstep fs os
                return $! (,) wr fr

------------------------------------------------------------------------------

data SurroundState = PrefixAdded | PrefixPending

{-| Adds a prefix and a suffix to the stream arriving into a 'Fold'.		

>>> L.fold (transduce (surround "prefix" "suffix") L.list) "middle"
"prefixmiddlesuffix"
-}
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

{-| Like 'surround', but the prefix and suffix are obtained using a 'IO'
    action.

>>> L.foldM (transduceM (surroundIO (return "prefix") (return "suffix")) (L.generalize L.list)) "middle"
"prefixmiddlesuffix"
-}
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

{-| Generalize a 'Transducer' to a 'TransducerM'.		

-}
generalizeTransducer :: Monad m => Transducer i o r -> TransducerM m i o r
generalizeTransducer (Transducer step begin done) = TransducerM step' begin' done'
  where
    step' x a = return (step x a)
    begin'    = return  begin
    done' x   = return (done x)

{-| Simplify a pure 'TransducerM' to a 'Transducer'.		

-}
simplifyTransducer :: TransducerM Identity i o r -> Transducer i o r
simplifyTransducer (TransducerM step begin done) = Transducer step' begin' done' where
    step' x a = runIdentity (step x a)
    begin'    = runIdentity  begin
    done' x   = runIdentity (done x)

{-| Transforms a 'Transducer' into a 'Fold' by forgetting about the data sent
    downstream.		

-}
foldify :: Transducer i o r -> Fold i r
foldify (Transducer step begin done) =
    Fold (\x i -> fst (step x i)) begin (\x -> fst (done x))

foldifyM :: Functor m => TransducerM m i o r -> FoldM m i r
foldifyM (TransducerM step begin done) =
    FoldM (\x i -> fmap fst (step x i)) begin (\x -> fmap fst (done x))

{-| Transforms a 'Fold' into a 'Transducer' that sends the return value of the
    'Fold' downstream when upstream closes.		

-}
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


{-| Changes the base monad used by a 'TransducerM'.		

-}
hoistTransducer :: Monad m => (forall a. m a -> n a) -> TransducerM m i o r -> TransducerM n i o r 
hoistTransducer g (TransducerM step begin done) = TransducerM (\s i -> g (step s i)) (g begin) (g . done)

{-| Changes the base monad used by a 'FoldM'.		

-}
hoistFold :: Monad m => (forall a. m a -> n a) -> FoldM m i r -> FoldM n i r 
hoistFold g (FoldM step begin done) = FoldM (\s i -> g (step s i)) (g begin) (g . done)

------------------------------------------------------------------------------

{-| A procedure for splitting a stream into delimited segments. It is
    composed of a step function, an initial state, and a /done/ function that
    may flush some accumulated output downstream.

    The step function returns a triplet of:

    * The new internal state.
    * Output that continues the last segment detected in the previous step.
    * A list of lists containing new segments detected in the current step. If
      the list is empty, that means no splitting has taken place in the current
      step.
-}
data Splitter i
     = forall x. Splitter (x -> i -> (x,[i],[[i]])) x (x -> [i])

{-| Applies a 'Transduction' to all groups detected by a 'Splitter', returning
    a 'Transduction' that works over the undivided stream of inputs.		

-}
groups :: Splitter i -> Transduction i b -> Transduction i b 
groups (Splitter sstep sbegin sdone) t f =
    Fold step (Pair sbegin (t (duplicated f))) done 
    where
        step (Pair ss fs) i = 
           let 
               (ss', oldSplit, newSplits) = sstep ss i
               fs' = foldl' (step' . reset) (step' fs oldSplit) newSplits
           in
           Pair ss' fs'
        step' = L.fold . duplicated
        reset (Fold _ fstate fdone) = 
           t (duplicated (fdone fstate)) 
        done (Pair ss (Fold fstep fstate fdone)) = 
            extract (fdone (foldl' fstep fstate (sdone ss)))

groupsM :: Monad m => Splitter i -> TransductionM m i b -> TransductionM m i b
groupsM (Splitter sstep sbegin sdone) t f = 
    FoldM step (return (Pair sbegin (t (duplicated f)))) done        
    where
        step (Pair ss fs) i = do
             let 
                 (ss', oldSplit, newSplits) = sstep ss i
             fs' <- step' fs oldSplit
             fs'' <- foldlM step'' fs' newSplits
             return $! Pair ss' fs''
        step' = L.foldM . duplicated
        step'' = \fs is -> reset fs >>= \fs' -> step' fs' is
        reset (FoldM _ fstate fdone) = 
           liftM (t . duplicated) (fstate >>= fdone) 
        done (Pair ss (FoldM fstep fstate fdone)) = do
            finalf <- fdone =<< flip (foldlM fstep) (sdone ss) =<< fstate
            L.foldM finalf [] 

{-| Summarizes each group detected by a 'Splitter' using a 'Fold', returning a
    'Transduction' that allows a 'Fold' to accept the original ungrouped input. 

-}
folds :: Splitter i -> Fold i b -> Transduction i b
folds splitter f = groups splitter (transduce (chokepoint f))

foldsM :: Splitter i -> FoldM m i b -> TransductionM m i b
foldsM splitter f = groupsM splitter (transduceM (chokepointM f))

------------------------------------------------------------------------------

{-| Splits a stream into chunks of fixed size.		

>>> L.fold (folds (chunksOf 2) L.list L.list) [1..7]
[[1,2],[3,4],[5,6],[7]]

>>> L.fold (groups (chunksOf 2) (transduce (surround [] [0])) L.list) [1..7]
[1,2,0,3,4,0,5,6,0,7,0]
-}
chunksOf :: Int -> Splitter a
chunksOf 0 = Splitter (\_ _ -> ((),[],repeat [])) () (error "never happens")
chunksOf groupSize = Splitter step groupSize done 
    where
        step 0 a = (pred groupSize, [], [[a]])
        step i a = (pred i, [a], [])
        done _ = []

------------------------------------------------------------------------------

{- $reexports

-}
