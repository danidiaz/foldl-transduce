{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

-- |
--
-- This module builds on module "Control.Foldl", adding stateful transducers
-- and grouping operations.

module Control.Foldl.Transduce (
        -- * Transducer types
        Transduction 
    ,   ReifiedTransduction (..)
    ,   Transduction' 
    ,   ReifiedTransduction' (..)
    ,   Transducer(..)
    ,   ToTransducer(..)
        -- ** Monadic transducer types
    ,   TransductionM
    ,   ReifiedTransductionM (..)
    ,   TransductionM'
    ,   ReifiedTransductionM' (..)
    ,   TransducerM(..)
    ,   ToTransducerM(..)
        -- * Applying transducers
    ,   transduce
    ,   transduce'
    ,   transduceM
    ,   transduceM'
        -- * Folding over groups
    ,   folds
    ,   folds'
    ,   foldsM
    ,   foldsM'
        -- * Other group operations
    ,   groups
    ,   evenly
    ,   bisect    
        --
    ,   groups'
    ,   evenly'
        --
    ,   groupsM
    ,   evenlyM
    ,   bisectM
        --
    ,   groupsM'
    ,   evenlyM'
        -- * Transducers
    ,   ignore
    ,   surround
    ,   surroundIO
        -- * Splitters
    ,   chunksOf
    ,   splitAt
    ,   chunkedSplitAt
    ,   splitWhen
    ,   splitLast
    ,   chunkedStripPrefix
        -- * Transducer utilities
    ,   foldify
    ,   foldifyM
    ,   condense
    ,   condenseM
    ,   hoistTransducer
        -- * Fold utilities
    ,   hoistFold
        -- * Re-exports
        -- $reexports
    ,   module Data.Functor.Extend
    ,   module Control.Foldl
    ,   module Control.Comonad.Cofree
    ) where

import Prelude hiding (take,drop,splitAt,dropWhile,unfold)

import Data.Bifunctor
import Data.Monoid
import qualified Data.Monoid.Cancellative as CM
import qualified Data.Monoid.Null as NM
import qualified Data.Monoid.Factorial as SFM
import Data.Functor.Identity
import Data.Functor.Extend
import Data.Foldable (Foldable,foldlM,foldl',toList)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Comonad
import Control.Comonad.Cofree 
import Control.Foldl (Fold(..),FoldM(..))
import qualified Control.Foldl as L
import Control.Foldl.Transduce.Internal (Pair(..),Trio(..),Quartet(..),_1of3)

{- $setup

>>> import qualified Control.Foldl as L
>>> import Control.Foldl.Transduce
>>> import Control.Applicative
>>> import qualified Control.Comonad.Cofree as C
>>> import Prelude hiding (take,drop,takeWhile,dropWhile)

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
        FoldM step begin (\x -> return $! FoldM step (return x) done)
    {-# INLINABLE duplicated #-}

------------------------------------------------------------------------------

{-| A (possibly stateful) transformation on the inputs of a 'Fold'.

    Functions constructed with combinators like 'L.premap' or 'L.handles' from
    "Control.Foldl" also typecheck as a 'Transduction'.
-}
type Transduction a b = forall x. Fold b x -> Fold a x

newtype ReifiedTransduction a b = ReifiedTransduction { getTransduction :: Transduction a b }

{-| A more general from of 'Transduction' that adds new information to the
    return value of the 'Fold'.

-}
type Transduction' a b r = forall x. Fold b x -> Fold a (r,x)

newtype ReifiedTransduction' a b r = ReifiedTransduction' { getTransduction' :: Transduction' a b r }

{-| A stateful process that transforms a stream of inputs into a stream of
    outputs, and may optionally demarcate groups in the stream of outputs.

    Composed of a step function, an initial state, and a extraction function. 

    The step function returns a triplet of:

    * The new internal state.
    * Outputs that continue the last segment detected in the previous step.
    * A list of lists containing outputs for segments detected in the current
      step. If the list is empty, that means no splitting has taken place in the
      current step. 'Transducer's that do not perform grouping never return anything
      other than @[]@ here. In effect, they treat the whole stream as a single group.

    The extraction function returns the 'Transducer's own result value, as
    well as any pending outputs.
-}
data Transducer i o r
     = forall x. Transducer (x -> i -> (x,[o],[[o]])) x (x -> (r,[o],[[o]]))

instance Comonad (Transducer i o) where
    extract (Transducer _ begin done) = _1of3 (done begin)
    {-# INLINABLE extract #-}

    duplicate (Transducer step begin done) = Transducer step begin (\x -> (Transducer step x done,[],[]))
    {-# INLINABLE duplicate #-}

instance Extend (Transducer i o) where
    duplicated f = duplicate f
    {-# INLINABLE duplicated #-}

instance Functor (Transducer i o) where
    fmap f (Transducer step begin done) = 
        Transducer 
            step 
            begin 
            ((\(x,xs,xss) -> (f x,xs,xss)) . done)

instance Bifunctor (Transducer i) where
    first f (Transducer step begin done) =
        Transducer 
            (fmap (\(x,xs,xss) -> (x,map f xs, map (map f) xss)) . step) 
            begin 
            ((\(x,xs,xss) -> (x,map f xs, map (map f) xss)) . done) 
    second f w = fmap f w

class ToTransducer t where
    toTransducer :: t i o r -> Transducer i o r

instance ToTransducer Transducer where
    toTransducer = id

instance ToTransducer (TransducerM Identity) where
    toTransducer = _simplify

{-| Like 'Transduction', but works on monadic 'Fold's.		

-}
type TransductionM m a b = forall x. Monad m => FoldM m b x -> FoldM m a x

newtype ReifiedTransductionM m a b = ReifiedTransductionM { getTransductionM :: TransductionM m a b }

type TransductionM' m a b r = forall x. FoldM m b x -> FoldM m a (r,x)

newtype ReifiedTransductionM' m a b r = ReifiedTransductionM' { getTransductionM' :: TransductionM' m a b r }

{-| Like 'Transducer', but monadic.

-}
data TransducerM m i o r
     = forall x. TransducerM (x -> i -> m (x,[o],[[o]])) (m x) (x -> m (r,[o],[[o]]))


instance Monad m => Functor (TransducerM m i o) where
    fmap f (TransducerM step begin done) = TransducerM step begin done'
      where
        done' x = do
            (r,os,oss) <- done x
            let r' = f r
            return $! (r' `seq` (r',os,oss))

instance (Functor m, Monad m) => Bifunctor (TransducerM m i) where
    first f (TransducerM step begin done) =
        TransducerM 
        (fmap (fmap (\(x,xs,xss) -> (x,map f xs, map (map f) xss))) . step) 
        begin 
        (fmap (\(x,xs,xss) -> (x,map f xs, map (map f) xss)) . done) 
    second f w = fmap f w

instance Monad m => Extend (TransducerM m i o) where
    duplicated (TransducerM step begin done) = 
        TransducerM step begin (\x -> return $! (TransducerM step (return x) done,[],[]))
    {-# INLINABLE duplicated #-}

class ToTransducerM m t where
    toTransducerM :: t i o r -> TransducerM m i o r

instance ToTransducerM m (TransducerM m) where
    toTransducerM = id

instance Monad m => ToTransducerM m Transducer where
    toTransducerM = _generalize

{-| Apply a 'Transducer' to a 'Fold', discarding the return value of the
    'Transducer'.		

>>> L.fold (transduce (Transducer (\_ i -> ((),[i],[])) () (\_ -> ('r',[],[]))) L.list) [1..7]
[1,2,3,4,5,6,7]
-}
transduce :: ToTransducer t => t i o s -> Transduction i o 
transduce t = fmap snd . (transduce' t)

{-| Generalized version of 'transduce' that preserves the return value of
    the 'Transducer'.

>>> L.fold (transduce' (Transducer (\_ i -> ((),[i],[])) () (\_ -> ('r',[],[]))) L.list) [1..7]
('r',[1,2,3,4,5,6,7])
-}
transduce' :: ToTransducer t => t i o s -> Transduction' i o s
transduce' (toTransducer -> Transducer wstep wstate wdone) (Fold fstep fstate fdone) =
    Fold step (Pair wstate fstate) done 
        where
            step (Pair ws fs) i = 
                let (ws',os,oss) = wstep ws i 
                in
                Pair ws' (foldl' fstep fs (os ++ mconcat oss))  
            done (Pair ws fs) = 
                let (wr,os,oss) = wdone ws
                in 
                (,) wr (fdone (foldl' fstep fs (os ++ mconcat oss)))


{-| Like 'transduce', but works on monadic 'Fold's.		

-}
transduceM :: (Monad m, ToTransducerM m t)  => t i o s -> TransductionM m i o 
transduceM t = fmap snd . (transduceM' t)

{-| Like 'transduce'', but works on monadic 'Fold's.		

-}
transduceM' :: (Monad m, ToTransducerM m t)  => t i o s -> TransductionM' m i o s
transduceM' (toTransducerM -> TransducerM wstep wstate wdone) (FoldM fstep fstate fdone) =
    FoldM step (liftM2 Pair wstate fstate) done 
        where
            step (Pair ws fs) i = do
                (ws',os,oss) <- wstep ws i
                fs' <- foldlM fstep fs (os ++ mconcat oss)
                return $! Pair ws' fs'
            done (Pair ws fs) = do
                (wr,os,oss) <- wdone ws
                fr <- fdone =<< foldlM fstep fs (os ++ mconcat oss)
                return $! (,) wr fr

------------------------------------------------------------------------------

{-| Polymorphic in both inputs and outputs.		

-}
ignore :: Transducer a b ()
ignore = 
    Transducer step () done 
    where
        step _ _ = 
            ((),[],[])
        done = 
            const ((),[],[])

data SurroundState = PrefixAdded | PrefixPending

{-| Adds a prefix and a suffix to the stream arriving into a 'Fold'.		

>>> L.fold (transduce (surround "prefix" "suffix") L.list) "middle"
"prefixmiddlesuffix"
-}
surround :: (Traversable p, Traversable s) => p a -> s a -> Transducer a a ()
surround (toList -> ps) (toList -> ss) = 
    Transducer step PrefixPending done 
    where
        step PrefixPending a = 
            (PrefixAdded, ps ++ [a],[])
        step PrefixAdded a = 
            (PrefixAdded, [a],[])
        done PrefixPending = 
            ((), ps ++ ss, [])
        done PrefixAdded = 
            ((), ss, [])

{-| Like 'surround', but the prefix and suffix are obtained using a 'IO'
    action.

>>> L.foldM (transduceM (surroundIO (return "prefix") (return "suffix")) (L.generalize L.list)) "middle"
"prefixmiddlesuffix"
-}
surroundIO :: (Traversable p, Traversable s, Functor m, MonadIO m) 
           => m (p a) 
           -> m (s a) 
           -> TransducerM m a a ()
surroundIO prefixa suffixa = 
    TransducerM step (return PrefixPending) done 
    where
        step PrefixPending a = do
            ps <- fmap toList prefixa
            return (PrefixAdded, ps ++ [a],[])
        step PrefixAdded a = 
            return (PrefixAdded, [a],[])
        done PrefixPending = do
            ps <- fmap toList prefixa
            ss <- fmap toList suffixa
            return ((), toList ps ++ toList ss, [])
        done PrefixAdded = do
            ss <- fmap toList suffixa
            return ((), toList ss , [])

------------------------------------------------------------------------------

{-| Generalize a 'Transducer' to a 'TransducerM'.		

-}
_generalize :: Monad m => Transducer i o s -> TransducerM m i o s
_generalize (Transducer step begin done) = TransducerM step' begin' done'
    where
    step' x a = return (step x a)
    begin'    = return  begin
    done' x   = return (done x)

{-| Simplify a pure 'TransducerM' to a 'Transducer'.		

-}
_simplify :: TransducerM Identity i o s -> Transducer i o s
_simplify (TransducerM step begin done) = Transducer step' begin' done' 
    where
    step' x a = runIdentity (step x a)
    begin'    = runIdentity  begin
    done' x   = runIdentity (done x)


{-| Transforms a 'Transducer' into a 'Fold' by forgetting about the data sent
    downstream.		

-}
foldify :: Transducer i o s -> Fold i s
foldify (Transducer step begin done) =
    Fold (\x i -> _1of3 (step x i)) begin (\x -> _1of3 (done x))

{-| Monadic version of 'foldify'.		

-}
foldifyM :: Functor m => TransducerM m i o s -> FoldM m i s
foldifyM (TransducerM step begin done) =
    FoldM (\x i -> fmap _1of3 (step x i)) begin (\x -> fmap _1of3 (done x))

{-| Transforms a 'Fold' into a 'Transducer' that sends the return value of the
    'Fold' downstream when upstream closes.		

-}
condense :: Fold a r -> Transducer a r r
condense (Fold fstep fstate fdone) =
    (Transducer wstep fstate wdone)
    where
        wstep = \fstate' i -> (fstep fstate' i,[],[])
        wdone = \fstate' -> (\r -> (r,[r],[])) (fdone fstate')

condenseM :: Applicative m => FoldM m a r -> TransducerM m a r r
condenseM (FoldM fstep fstate fdone) = 
    (TransducerM wstep fstate wdone)
    where
        wstep = \fstate' i -> fmap (\s -> (s,[],[])) (fstep fstate' i)
        wdone = \fstate' -> fmap (\r -> (r,[r],[])) (fdone fstate')


{-| Changes the base monad used by a 'TransducerM'.		

-}
hoistTransducer :: Monad m => (forall a. m a -> n a) -> TransducerM m i o s -> TransducerM n i o s 
hoistTransducer g (TransducerM step begin done) = TransducerM (\s i -> g (step s i)) (g begin) (g . done)

{-| Changes the base monad used by a 'FoldM'.		

-}
hoistFold :: Monad m => (forall a. m a -> n a) -> FoldM m i r -> FoldM n i r 
hoistFold g (FoldM step begin done) = FoldM (\s i -> g (step s i)) (g begin) (g . done)

------------------------------------------------------------------------------

{-| Repeatedly applies a 'Transduction' to process each of the groups
    demarcated by a 'Transducer', returning a 'Fold' what works over the
    undivided stream of inputs. 
    
    The return value of the 'Transducer' is discarded.

>>> L.fold (groups (chunksOf 2) (transduce (surround "<" ">")) L.list) "aabbccdd"
"<aa><bb><cc><dd>"
-}
--groups :: Transducer a b s -> Transduction b c -> Transduction a c 
--groups splitter transduction oldfold = 
--    let transduction' = fmap ((,) ()) . transduction
--        newfold = groups' splitter L.mconcat transduction' oldfold 
--    in 
--    fmap snd newfold

{-| Like 'groups', but applies a different transduction to each group. 		

>>> :{ 
    let 
      transducers = flip C.unfold 0 $ \i -> (,)
         (ReifiedTransduction (transduce (surround (show i) []))) 
         (Identity (succ i))
    in L.fold (groupsVarying (chunksOf 2) transducers L.list) "aabbccdd"
    :}
"0aa1bb2cc3dd"
-}
groups :: ToTransducer t 
       => t a b s 
       -> Cofree Identity (ReifiedTransduction b c) -- ^ infinite list of transductions
       -> Transduction a c 
groups splitter transductions oldfold = 
    let transductions' = 
              fmap (\rt -> 
                        (ReifiedTransduction' (fmap (fmap ((,) ())) (getTransduction rt))))
            . hoistCofree (const . runIdentity)
            $ transductions 
        newfold = groups' splitter L.mconcat transductions' oldfold 
    in 
    fmap snd newfold

evenly :: Transduction b c -> Cofree Identity (ReifiedTransduction b c) 
evenly = coiter Identity . ReifiedTransduction 

bisect :: ToTransducer t 
       => t a b s 
       -> Transduction b c -- ^ head
       -> Transduction b c 
       -> Transduction a c 
bisect t t0 t1 = groups t (ReifiedTransduction t0 :< Identity (evenly t1))

{-| Generalized version of 'groups' that preserves the return value of the
    'Transducer'.

    A summary value for each group is also calculated. They are aggregated for
    the whole stream, with the help of an auxiliary 'Fold'.

>>> L.fold (groups' (chunksOf 2) L.list (\f -> transduce (surround "<" ">") (liftA2 (,) L.list f)) L.list) "aabbccdd"
(((),["<aa>","<bb>","<cc>","<dd>"]),"<aa><bb><cc><dd>")
-}
--groups' :: Transducer a b s
--        -> Fold u v -- ^ auxiliary 'Fold' that aggregates the @u@ values produced for each group
--        -> Transduction' b c u -- ^ repeatedly applied for processing each group
--        -> Transduction' a c (s,v) 
--groups' splitter summarizer transduction somefold =
--    let transductions = coiter const (ReifiedTransduction' transduction)
--    in  groupsVarying' splitter summarizer transductions somefold

groups' :: ToTransducer t
        => t a b s
        -> Fold u v -- ^ auxiliary 'Fold' that aggregates the @u@ values produced for each group
        -> Cofree ((->) u) (ReifiedTransduction' b c u) -- ^ a machine that eats @u@ values and spits transductions
        -> Transduction' a c (s,v) 
groups' (toTransducer -> Transducer sstep sbegin sdone) somesummarizer (ReifiedTransduction' t0 :< somemachine) somefold =
    Fold step (Quartet sbegin somesummarizer (t0 (duplicated somefold)) somemachine) done 
      where 
        step (Quartet sstate summarizer innerfold machine) i =
           let 
               (sstate', oldSplit, newSplits) = sstep sstate i
               (summarizer',innerfold',machine') = 
                   foldl' 
                   step'
                   (summarizer, feed innerfold oldSplit,machine) 
                   newSplits
           in
           Quartet sstate' summarizer' innerfold' machine'
        
        step' (summarizer_,innerfold_,machine_) somesplit = 
           let (u,resetted,nextmachine) = reset machine_ innerfold_
           in  (L.fold (duplicated summarizer_) [u], feed resetted somesplit,nextmachine)

        feed = L.fold . duplicated
        reset machine (Fold _ fstate fdone) = 
            let (u,nextfold) = fdone fstate
                ReifiedTransduction' t1 :< nextmachine = machine u
            in  (u,t1 (duplicated nextfold),nextmachine)
        done (Quartet sstate summarizer innerfold machine) = 
            let 
                (s,oldSplit,newSplits) = sdone sstate
                (summarizer',innerfold',_) = 
                   foldl' 
                   step'
                   (summarizer,feed innerfold oldSplit,machine) 
                   newSplits
                (u,finalfold) = extract innerfold'
            in  ((s,L.fold summarizer' [u]),extract finalfold)

evenly' :: Transduction' b c u -> Cofree ((->) u) (ReifiedTransduction' b c u) 
evenly' = coiter const . ReifiedTransduction' 


{-| Monadic version of 'groups'.		

-}
--groupsM :: Monad m => TransducerM m a b s -> TransductionM m b c -> TransductionM m a c
--groupsM splitter transduction oldfold = 
--    let transduction' = fmap ((,) ()) . transduction
--        newfold = 
--            groupsM' splitter (L.generalize L.mconcat) transduction' oldfold 
--    in 
--    fmap snd newfold

groupsM :: (Monad m, ToTransducerM m t)
               => t a b s -- ^
               -> Cofree Identity (ReifiedTransductionM m b c)
               -> TransductionM m a c
groupsM splitter transductions oldfold = 
    let transductions' = 
              fmap (\rt -> 
                        ReifiedTransductionM'
                        (fmap (fmap ((,) ())) (getTransductionM rt)))
            . hoistCofree (const . runIdentity)
            $ transductions 
        newfold = groupsM' splitter (L.generalize L.mconcat) transductions' oldfold 
    in 
    fmap snd newfold

evenlyM :: TransductionM m b c -> Cofree Identity (ReifiedTransductionM m b c) 
evenlyM = coiter Identity . ReifiedTransductionM


bisectM :: ToTransducerM m t 
        => t a b s 
        -> TransductionM m b c -- ^ head
        -> TransductionM m b c 
        -> TransductionM m a c 
bisectM t t0 t1 = groupsM t (ReifiedTransductionM t0 :< Identity (evenlyM t1))

--wrapM :: TransductionM m b c -> Cofree Identity (ReifiedTransductionM m b c) -> Cofree Identity (ReifiedTransductionM m b c) 
--wrapM t s = ReifiedTransductionM t :< Identity s

{-| Monadic version of 'groups''.		

-}
--groupsM' :: Monad m 
--         => TransducerM m a b s -- ^
--         -> FoldM m u v 
--         -> TransductionM' m b c u 
--         -> TransductionM' m a c (s,v) 
--groupsM' splitter summarizer transduction somefold =
--    let transductions = coiter const (ReifiedTransductionM' transduction)
--    in  groupsVaryingM' splitter summarizer transductions somefold

groupsM' :: (Monad m, ToTransducerM m t) 
         => t a b s 
         -> FoldM m u v 
         -> Cofree ((->) u) (ReifiedTransductionM' m b c u) -- ^ a machine that eats @u@ values and spits transductions
         -> TransductionM' m a c (s,v) 

groupsM' (toTransducerM -> TransducerM sstep sbegin sdone) somesummarizer (ReifiedTransductionM' t0 :< somemachine) somefold =
    FoldM step (sbegin >>= \x -> return (Quartet x somesummarizer (t0 (duplicated somefold)) somemachine)) done        
    where
        step (Quartet sstate summarizer innerfold machine) i = do
            (sstate', oldSplit, newSplits) <- sstep sstate i 
            innerfold' <- feed innerfold oldSplit
            (summarizer',innerfold'',machine') <- foldlM step' (summarizer,innerfold',machine) newSplits
            return $! Quartet sstate' summarizer' innerfold'' machine'

        step' = \(summarizer,innerfold,machine) is -> do
            (u,innerfold',machine') <- reset machine innerfold 
            summarizer' <- L.foldM (duplicated summarizer) [u]
            innerfold'' <- feed innerfold' is
            return $! (summarizer',innerfold'',machine') 

        feed = L.foldM . duplicated

        reset machine (FoldM _ fstate fdone) = do
           (u,nextfold) <- fdone =<< fstate 
           let 
               ReifiedTransductionM' t1 :< nextmachine = machine u
           return (u,t1 (duplicated nextfold),nextmachine)

        done (Quartet sstate summarizer innerfold machine) = do
            (s,oldSplit,newSplits) <- sdone sstate
            innerfold' <- feed innerfold oldSplit
            (summarizer',innerfold'',_) <- foldlM step' (summarizer,innerfold',machine) newSplits
            (u,finalfold) <- L.foldM innerfold'' []
            v <- L.foldM summarizer' [u]
            r <- L.foldM finalfold []
            return ((s,v),r)


evenlyM' :: TransductionM' m b c u -> Cofree ((->) u) (ReifiedTransductionM' m b c u) 
evenlyM' = coiter const . ReifiedTransductionM'

{-| Summarizes each of the groups demarcated by the 'Transducer' using a
    'Fold'. 
    
    The result value of the 'Transducer' is discarded.

>>> L.fold (folds (chunksOf 3) L.sum L.list) [1..7]
[6,15,7]
-}
folds :: ToTransducer t => t a b s -> Fold b c -> Transduction a c
folds splitter f = groups splitter (evenly (transduce (condense f)))

--foldsVarying :: Transducer a b s 
--             -> Cofree Identity (Fold b c) -- ^ infinite list of 'Fold's.
--             -> Transduction a c
--foldsVarying splitter foldlist = groupsVarying splitter transducers
--    where
--    foldToTrans f = ReifiedTransduction (transduce (condense f))
--    transducers = fmap foldToTrans foldlist 

{-| Like 'folds', but preserves the return value of the 'Transducer'.

>>> L.fold (folds' (chunksOf 3) L.sum L.list) [1..7]
((),[6,15,7])
-}
folds' :: ToTransducer t => t a b s -> Fold b c -> Transduction' a c s
folds' splitter innerfold somefold = 
    fmap (bimap fst id) (groups' splitter L.mconcat (evenly' innertrans) somefold)
    where
    innertrans = fmap ((,) ()) . transduce (condense innerfold)
--
--foldsVarying' :: Transducer a b s 
--              -> Cofree ((->) c) (Fold b c)
--              -> Transduction' a c s
--foldsVarying' splitter foldlist somefold = 
--    fmap fst $ groupsVarying' splitter somefold transducers (pure ())
--    where
--    foldToTrans f = ReifiedTransduction' (transduce' (condense f))
--    transducers = fmap foldToTrans foldlist 

{-| Monadic version of 'folds'.		

-}
foldsM :: (Applicative m, Monad m, ToTransducerM m t) => t a b s -> FoldM m b c -> TransductionM m a c
foldsM splitter f = groupsM splitter (evenlyM (transduceM (condenseM f)))

--foldsVaryingM :: (Applicative m, Monad m) 
--              => TransducerM m a b s 
--              -> Cofree Identity (FoldM m b c) -- ^ infinite list of 'FoldM's.
--              -> TransductionM m a c
--foldsVaryingM splitter foldlist = groupsVaryingM splitter transducers
--    where
--    foldToTrans f = ReifiedTransductionM (transduceM (condenseM f))
--    transducers = fmap foldToTrans foldlist 

--foldsVaryingM' :: (Applicative m, Monad m)
--               => TransducerM m a b s 
--               -> Cofree ((->) c) (FoldM m b c)
--               -> TransductionM' m a c s
--foldsVaryingM' splitter foldlist somefold = 
--    fmap fst $ groupsVaryingM' splitter somefold transducers (pure ())
--    where
--    foldToTrans f = ReifiedTransductionM' (transduceM' (condenseM f))
--    transducers = fmap foldToTrans foldlist 
--
--{-| Monadic version of 'folds''.		
--
---}
foldsM' :: (Applicative m,Monad m, ToTransducerM m t) => t a b s -> FoldM m b c -> TransductionM' m a c s
foldsM' splitter innerfold somefold = 
    fmap (bimap fst id) (groupsM' splitter (L.generalize L.mconcat) (evenlyM' innertrans) somefold)
    where
    innertrans = fmap ((,) ()) . transduceM (condenseM innerfold)

------------------------------------------------------------------------------

{-| Splits a stream into chunks of fixed size.		

>>> L.fold (folds (chunksOf 2) L.list L.list) [1..7]
[[1,2],[3,4],[5,6],[7]]

>>> L.fold (groups (chunksOf 2) (transduce (surround [] [0])) L.list) [1..7]
[1,2,0,3,4,0,5,6,0,7,0]
-}
chunksOf :: Int -> Transducer a a ()
chunksOf 0 = Transducer (\_ _ -> ((),[],repeat [])) () (error "never happens")
chunksOf groupSize = Transducer step groupSize done 
    where
        step 0 a = (pred groupSize, [], [[a]])
        step i a = (pred i, [a], [])
        done _ = ((),[],[])

{-| Pass the first @n@ inputs to the 'Fold', and ignore the rest.		

>>> L.fold (transduce (take 2) L.list) [1..5]
[1,2]

>>> L.fold (transduce (take 0) L.list) [1..5]
[]
-}
splitAt :: Int -> Transducer a a ()
splitAt howmany = 
    Transducer step (Just howmany) done 
    where
        step Nothing i =
            (Nothing,[i],[])
        step (Just howmanypending) i 
            | howmanypending == 0 = 
                (Nothing,[],[[i]])
            | otherwise = 
                (Just (pred howmanypending),[i],[]) 
        done = mempty

chunkedSplitAt :: SFM.StableFactorialMonoid m => Int -> Transducer m m ()
chunkedSplitAt howmany = 
    Transducer step (Just howmany) done
    where
        step Nothing m =
            (Nothing,[m],[])
        step (Just howmanypending) m
            | NM.null m = 
                (Just howmanypending,[],[])
            | howmanypending == 0 = 
                (Nothing,[],[[m]])
            | howmanypending >= SFM.length m =
                (Just (howmanypending - SFM.length m),[m],[])
            | otherwise =
                let (prefix,suffix) = SFM.splitAt howmanypending m
                in
                (Nothing,[prefix],[[suffix]])
        done = mempty

data SplitWhenWhenState = 
      SplitWhenConditionEncountered 
    | SplitWhenConditionPending

{-| 		

>>> L.fold (transduce (takeWhile (<3)) L.list) [1..5]
[1,2]
-}
splitWhen :: (a -> Bool) -> Transducer a a ()
splitWhen predicate = 
    Transducer step SplitWhenConditionPending done 
    where
        step SplitWhenConditionPending i = 
            if predicate i 
               then (SplitWhenConditionEncountered,[],[[i]])
               else (SplitWhenConditionPending,[i],[])
        step SplitWhenConditionEncountered i = 
               (SplitWhenConditionEncountered,[i],[])
        done = mempty

splitLast :: Transducer a a (Maybe a)
splitLast =
    Transducer step Nothing done
    where
        step Nothing i = 
            (Just i,[],[])
        step (Just oldi) i = 
            (Just i,[oldi],[])
        done Nothing = 
            (Nothing,[],[])
        done (Just lasti) = (Just lasti, [], [[lasti]])

chunkedStripPrefix :: (CM.LeftGCDMonoid i,SFM.StableFactorialMonoid i,Traversable t,Monad m) => t i -> TransducerM (ExceptT () m) i i ()
chunkedStripPrefix (filter (not . NM.null) . toList -> chunks) = 
    TransducerM step (return chunks) done
    where
        step [] i = return ([],[i],[])
        step xss@(x:xs) i 
            | NM.null i = return (xss,[],[])
            | otherwise = undefined
        done [] = return mempty
        done (_:_) = throwE () 

{-| Ignore the firs @n@ inputs, pass all subsequent inputs to the 'Fold'.		

>>> L.fold (transduce (drop 2) L.list) [1..5]
[3,4,5]

>>> L.fold (transduce (drop 0) L.list) [1..5]
[1,2,3,4,5]
-}

{-| 		

>>> L.fold (transduce (dropWhile (<3)) L.list) [1..5]
[3,4,5]
-}

------------------------------------------------------------------------------

{- $reexports

-}
