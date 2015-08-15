{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Control.Foldl.Transduce (
        Transducer 
    ,   TransducerM
    ,   TransducerG
    ,   Wrap(..)
    ,   WrapM(..)
    ,   transducer
    ,   transducer'
    ,   transducerM
    ,   transducerM'
    ,   generalizeWrap
    ,   simplifyWrap
    ,   foldify
    ,   foldifyM
    ,   chunksOf
    ,   module Control.Foldl
    ) where

import Data.Bifunctor
import Data.Functor.Identity
import Data.Foldable (foldrM)
import Control.Monad
import Control.Foldl (Fold(..),FoldM(..))
import qualified Control.Foldl as L

data Pair a b = Pair !a !b

type Transducer a b = forall x. L.Fold b x -> L.Fold a x

type TransducerM m a b = forall x. Monad m => L.FoldM m b x -> L.FoldM m a x

type TransducerG a b = forall x m. Monad m => L.FoldM m b x -> L.FoldM m a x

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

transducer :: Wrap i o r -> Transducer i o 
transducer = transducer' (flip const) 

transducer' :: (x -> y -> z) -> Wrap i o x -> Fold o y -> Fold i z
transducer' f (Wrap wstep wstate wdone) (Fold fstep fstate fdone) =
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


transducerM :: Monad m => WrapM m i o r -> TransducerM m i o 
transducerM = transducerM' (flip const)

transducerM' :: Monad m => (x -> y -> z) -> WrapM m i o x -> FoldM m o y -> FoldM m i z
transducerM' f (WrapM wstep wstate wdone) (FoldM fstep fstate fdone) =
    FoldM step (liftM2 Pair wstate fstate) done 
        where
            step (Pair ws fs) i = do
                (ws',os) <- wstep ws i
                liftM (Pair ws') (foldrM (flip fstep) fs os)
            done (Pair ws fs) = do
                (wr,os) <- wdone ws
                liftM (f wr) (fdone =<< foldrM (flip fstep) fs os)


generalizeWrap :: Monad m => Wrap i o r -> WrapM m i o r
generalizeWrap (Wrap step begin done) = WrapM step' begin' done'
  where
    step' x a = return (step x a)
    begin'    = return  begin
    done' x   = return (done x)

simplifyWrap :: WrapM Identity i o r -> Wrap i o r
simplifyWrap (WrapM step begin done) = Wrap step' begin' done'
  where
    step' x a = runIdentity (step x a)
    begin'    = runIdentity  begin
    done' x   = runIdentity (done x)

foldify :: Wrap i o r -> Fold i r
foldify (Wrap step begin done) =
    Fold (\x i -> fst (step x i)) begin (\x -> fst (done x))

foldifyM :: Functor m => WrapM m i o r -> FoldM m i r
foldifyM (WrapM step begin done) =
    FoldM (\x i -> fmap fst (step x i)) begin (\x -> fmap fst (done x))

chunksOf :: Int -> Transducer a [a]
chunksOf groupSize = transducer $ 
    Wrap step (Pair 0 []) done 
    where
        step (Pair i as) a = 
            let i' = succ i 
                as' = a:as
            in
            if (i' == groupSize)
               then (Pair 0 [], [reverse as'])
               else (Pair i' as', [])

        done (Pair _ []) = ((), [])
        done (Pair _ as) = ((), [reverse as])
