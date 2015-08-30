{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

-- |
--
-- This module builds on module "Control.Foldl", adding stateful transducers
-- and grouping operations.

module Control.Foldl.Transduce (
        -- * Transducer types
        Transducer(..)
    ,   Transduction 
    ,   Transduction' 
    ,   ReifiedTransduction' (..)
    ,   TransducerM(..)
    ,   TransductionM
    ,   TransductionM'
    ,   ReifiedTransductionM' (..)
        -- * Applying transducers
    ,   transduce
    ,   transduce'
    ,   transduceM
    ,   transduceM'
        -- * Working with groups
    ,   groups
    ,   groupsVarying
    ,   groups'
    ,   groupsVarying'
    ,   groupsM
    ,   groupsVaryingM
    ,   groupsM'
    ,   groupsVaryingM'
    ,   folds
    ,   foldsVarying
    ,   folds'
    ,   foldsM
    ,   foldsM'
        -- * Transducers
    ,   take
    ,   takeWhile
    ,   drop
    ,   dropWhile
    ,   surround
    ,   surroundIO
        -- * Splitters
    ,   chunksOf
        -- * Transducer utilities
    ,   _generalize
    ,   _simplify
    ,   foldify
    ,   foldifyM
    ,   hoistTransducer
    ,   hoistFold
        -- * Re-exports
        -- $reexports
    ,   module Data.Functor.Extend
    ,   module Control.Foldl
    ) where

import Prelude hiding (take,drop,takeWhile,dropWhile,unfold)

import Data.Bifunctor
import Data.Monoid
import Data.Functor.Identity
import Data.Functor.Extend
import Data.Foldable (Foldable,foldlM,foldl',toList)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Comonad
import Control.Comonad.Cofree
import Control.Foldl (Fold(..),FoldM(..))
import qualified Control.Foldl as L
import Control.Foldl.Transduce.Internal (Pair(..),Trio(..),Quartet(..),_1of3)

{- $setup

>>> import qualified Control.Foldl as L
>>> import Control.Foldl.Transduce
>>> import Control.Applicative
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

{-| A more general from of 'Transduction' that adds new information to the
    return value of the 'Fold'.

-}
type Transduction' a b r = forall x. Fold b x -> Fold a (r,x)

newtype ReifiedTransduction' a b r = ReifiedTransduction' { getTransduction' :: forall x. Fold b x -> Fold a (r,x) }

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
     = forall x. Transducer (x -> i -> (x,[o],[[o]])) x (x -> (r,[o]))

instance Comonad (Transducer i o) where
    extract (Transducer _ begin done) = fst (done begin)
    {-# INLINABLE extract #-}

    duplicate (Transducer step begin done) = Transducer step begin (\x -> (Transducer step x done,[]))
    {-# INLINABLE duplicate #-}

instance Extend (Transducer i o) where
    duplicated f = duplicate f
    {-# INLINABLE duplicated #-}

instance Functor (Transducer i o) where
    fmap f (Transducer step begin done) = Transducer step begin (first f . done)

instance Bifunctor (Transducer i) where
    first f (Transducer step begin done) =
        Transducer (fmap (\(x,xs,xss) -> (x,map f xs, map (map f) xss)) . step) begin (fmap (fmap f) . done)
    second f w = fmap f w

{-| Like 'Transduction', but works on monadic 'Fold's.		

-}
type TransductionM m a b = forall x. Monad m => FoldM m b x -> FoldM m a x

type TransductionM' m a b r = forall x. FoldM m b x -> FoldM m a (r,x)

newtype ReifiedTransductionM' m a b r = ReifiedTransductionM' { getTransductionM' :: TransductionM' m a b r }

{-| Like 'Transducer', but monadic.

-}
data TransducerM m i o r
     = forall x. TransducerM (x -> i -> m (x,[o],[[o]])) (m x) (x -> m (r,[o]))

instance Monad m => Functor (TransducerM m i o) where
    fmap f (TransducerM step begin done) = TransducerM step begin done'
      where
        done' x = do
            (r,os) <- done x
            let r' = f r
            return $! (r' `seq` (r', os))

instance (Functor m, Monad m) => Bifunctor (TransducerM m i) where
    first f (TransducerM step begin done) =
        TransducerM (fmap (fmap (\(x,xs,xss) -> (x,map f xs, map (map f) xss))) . step) begin (fmap (fmap (fmap f)) . done)
    second f w = fmap f w

instance Monad m => Extend (TransducerM m i o) where
    duplicated (TransducerM step begin done) = 
        TransducerM step begin (\x -> return $! (TransducerM step (return x) done,[]))
    {-# INLINABLE duplicated #-}

{-| Apply a 'Transducer' to a 'Fold', discarding the return value of the
    'Transducer'.		

>>> L.fold (transduce (Transducer (\_ i -> ((),[i],[])) () (\_ -> ('r',[]))) L.list) [1..7]
[1,2,3,4,5,6,7]
-}
transduce :: Transducer i o s -> Transduction i o 
transduce t = fmap snd . (transduce' t)

{-| Generalized version of 'transduce' that preserves the return value of
    the 'Transducer'.

>>> L.fold (transduce' (Transducer (\_ i -> ((),[i],[])) () (\_ -> ('r',[]))) L.list) [1..7]
('r',[1,2,3,4,5,6,7])
-}
transduce' :: Transducer i o s -> Transduction' i o s
transduce' (Transducer wstep wstate wdone) (Fold fstep fstate fdone) =
    Fold step (Pair wstate fstate) done 
        where
            step (Pair ws fs) i = 
                let (ws',os,oss) = wstep ws i 
                in
                Pair ws' (foldl' fstep fs (os ++ mconcat oss))  
            done (Pair ws fs) = 
                let (wr,os) = wdone ws
                in 
                (,) wr (fdone (foldl' fstep fs os))


{-| Like 'transduce', but works on monadic 'Fold's.		

-}
transduceM :: Monad m => TransducerM m i o s -> TransductionM m i o 
transduceM t = fmap snd . (transduceM' t)

{-| Like 'transduce'', but works on monadic 'Fold's.		

-}
transduceM' :: Monad m => TransducerM m i o s -> TransductionM' m i o s
transduceM' (TransducerM wstep wstate wdone) (FoldM fstep fstate fdone) =
    FoldM step (liftM2 Pair wstate fstate) done 
        where
            step (Pair ws fs) i = do
                (ws',os,oss) <- wstep ws i
                fs' <- foldlM fstep fs (os ++ mconcat oss)
                return $! Pair ws' fs'
            done (Pair ws fs) = do
                (wr,os) <- wdone ws
                fr <- fdone =<< foldlM fstep fs os
                return $! (,) wr fr

------------------------------------------------------------------------------

{-| Pass the first @n@ inputs to the 'Fold', and ignore the rest.		

>>> L.fold (transduce (take 2) L.list) [1..5]
[1,2]

>>> L.fold (transduce (take 0) L.list) [1..5]
[]
-}
take :: Int -> Transducer a a ()
take howmany = 
    Transducer step howmany done 
    where
        step howmanypending i 
            | howmanypending == 0 = 
                (0,[],[])
            | otherwise = 
                (pred howmanypending,[i],[]) 
        done = const ((),[])

{-| 		

>>> L.fold (transduce (takeWhile (<3)) L.list) [1..5]
[1,2]
-}
takeWhile :: (a -> Bool) -> Transducer a a ()
takeWhile predicate = 
    Transducer step False done 
    where
        step False i = 
            if predicate i 
               then (False,[i],[])
               else (True,[],[])
        step True _ = 
               (True,[],[])
        done = const ((),[])

{-| Ignore the firs @n@ inputs, pass all subsequent inputs to the 'Fold'.		

>>> L.fold (transduce (drop 2) L.list) [1..5]
[3,4,5]

>>> L.fold (transduce (drop 0) L.list) [1..5]
[1,2,3,4,5]
-}
drop :: Int -> Transducer a a ()
drop howmany = 
    Transducer step howmany done 
    where
        step howmanypending i 
            | howmanypending == 0 = 
                (0,[i],[]) 
            | otherwise = 
                (pred howmanypending,[],[])
        done = const ((),[])

{-| 		

>>> L.fold (transduce (dropWhile (<3)) L.list) [1..5]
[3,4,5]
-}
dropWhile :: (a -> Bool) -> Transducer a a ()
dropWhile predicate = 
    Transducer step False done 
    where
        step False i = 
            if predicate i 
               then (False,[],[])
               else (True,[i],[])
        step True i = 
               (True,[i],[])
        done = const ((),[])

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
            (PrefixAdded, ps ++ [a],[])
        step PrefixAdded a = 
            (PrefixAdded, [a],[])
        done PrefixPending = 
            ((), ps ++ ss)
        done PrefixAdded = 
            ((), ss)

{-| Like 'surround', but the prefix and suffix are obtained using a 'IO'
    action.

>>> L.foldM (transduceM (surroundIO (return "prefix") (return "suffix")) (L.generalize L.list)) "middle"
"prefixmiddlesuffix"
-}
surroundIO :: (Foldable p, Foldable s, Functor m, MonadIO m) 
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
            return ((), toList ps ++ toList ss)
        done PrefixAdded = do
            ss <- fmap toList suffixa
            return ((), toList ss)

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
    Fold (\x i -> _1of3 (step x i)) begin (\x -> fst (done x))

{-| Monadic version of 'foldify'.		

-}
foldifyM :: Functor m => TransducerM m i o s -> FoldM m i s
foldifyM (TransducerM step begin done) =
    FoldM (\x i -> fmap _1of3 (step x i)) begin (\x -> fmap fst (done x))

{-| Transforms a 'Fold' into a 'Transducer' that sends the return value of the
    'Fold' downstream when upstream closes.		

-}
chokepoint :: Fold a r -> Transducer a r ()
chokepoint (Fold fstep fstate fdone) =
    (Transducer wstep fstate wdone)
    where
        wstep = \fstate' i -> (fstep fstate' i,[],[])
        wdone = \fstate' -> ((),[fdone fstate'])

chokepointM :: Applicative m => FoldM m a r -> TransducerM m a r ()
chokepointM (FoldM fstep fstate fdone) = 
    (TransducerM wstep fstate wdone)
    where
        wstep = \fstate' i -> fmap (\s -> (s,[],[])) (fstep fstate' i)
        wdone = \fstate' -> fmap (\r -> ((),[r])) (fdone fstate')


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
groups :: Transducer a b s -> Transduction b c -> Transduction a c 
groups splitter transduction oldfold = 
    let transduction' = fmap ((,) ()) . transduction
        newfold = groups' splitter L.mconcat transduction' oldfold 
    in 
    fmap snd newfold

groupsVarying :: Transducer a b s 
              -> Cofree Identity (ReifiedTransduction' b c ()) 
              -> Transduction a c 
groupsVarying splitter transductions oldfold = 
    let transductions' = 
              hoistCofree (const . runIdentity)
            $ transductions 
        newfold = groupsVarying' splitter L.mconcat transductions' oldfold 
    in 
    fmap snd newfold

{-| Generalized version of 'groups' that preserves the return value of the
    'Transducer'.

    A summary value for each group is also calculated. They are aggregated for
    the whole stream, with the help of an auxiliary 'Fold'.

>>> L.fold (groups' (chunksOf 2) L.list (\f -> transduce (surround "<" ">") (liftA2 (,) L.list f)) L.list) "aabbccdd"
(((),["<aa>","<bb>","<cc>","<dd>"]),"<aa><bb><cc><dd>")
-}
groups' :: Transducer a b s
        -> Fold u v -- ^ auxiliary 'Fold' that aggregates the @u@ values produced for each group
        -> Transduction' b c u -- ^ repeatedly applied for processing each group
        -> Transduction' a c (s,v) 
groups' splitter summarizer transduction somefold =
    let transductions = coiter const (ReifiedTransduction' transduction)
    in  groupsVarying' splitter summarizer transductions somefold

groupsVarying' :: Transducer a b s
               -> Fold u v -- ^ auxiliary 'Fold' that aggregates the @u@ values produced for each group
               -> Cofree ((->) u) (ReifiedTransduction' b c u) -- ^ a machine that eats @u@ values and spits transductions
               -> Transduction' a c (s,v) 
groupsVarying' (Transducer sstep sbegin sdone) somesummarizer (ReifiedTransduction' t0 :< somemachine) somefold =
    Fold step (Quartet sbegin somesummarizer (t0 (duplicated somefold)) somemachine) done 
      where 
        step (Quartet sstate summarizer innerfold machine) i =
           let 
               (sstate', oldSplit, newSplits) = sstep sstate i
               (summarizer',innerfold',machine') = 
                   foldl' 
                   (\(summarizer_,innerfold_,machine_) somesplit -> 
                       let (u,resetted,nextmachine) = reset machine_ innerfold_
                       in  (L.fold (duplicated summarizer_) [u], feed resetted somesplit,nextmachine))
                   (summarizer, feed innerfold oldSplit,machine) 
                   newSplits
           in
           Quartet sstate' summarizer' innerfold' machine'
        feed = L.fold . duplicated
        reset machine (Fold _ fstate fdone) = 
            let (u,nextfold) = fdone fstate
                ReifiedTransduction' t1 :< nextmachine = machine u
            in  (u,t1 (duplicated nextfold),nextmachine)
        done (Quartet sstate summarizer (Fold fstep fstate fdone) _) = 
            let 
                (s,bss) = sdone sstate
                (u,extract -> x) = fdone (foldl' fstep fstate bss)
            in  ((s,L.fold summarizer [u]),x)

{-| Monadic version of 'groups'.		

-}
groupsM :: Monad m => TransducerM m a b s -> TransductionM m b c -> TransductionM m a c
groupsM splitter transduction oldfold = 
    let transduction' = fmap ((,) ()) . transduction
        newfold = 
            groupsM' splitter (L.generalize L.mconcat) transduction' oldfold 
    in 
    fmap snd newfold

groupsVaryingM :: Monad m 
               => TransducerM m a b s 
               -> Cofree Identity (ReifiedTransductionM' m b c ())
               -> TransductionM m a c
groupsVaryingM splitter transductions oldfold = 
    let transductions' = 
              hoistCofree (const . runIdentity)
            $ transductions 
        newfold = groupsVaryingM' splitter (L.generalize L.mconcat) transductions' oldfold 
    in 
    fmap snd newfold

{-| Monadic version of 'groups''.		

-}
groupsM' :: Monad m 
         => TransducerM m a b s -- ^
         -> FoldM m u v 
         -> TransductionM' m b c u 
         -> TransductionM' m a c (s,v) 
groupsM' splitter summarizer transduction somefold =
    let transductions = coiter const (ReifiedTransductionM' transduction)
    in  groupsVaryingM' splitter summarizer transductions somefold

groupsVaryingM' :: Monad m 
                => TransducerM m a b s 
                -> FoldM m u v 
                -> Cofree ((->) u) (ReifiedTransductionM' m b c u) -- ^ a machine that eats @u@ values and spits transductions
                -> TransductionM' m a c (s,v) 

groupsVaryingM' (TransducerM sstep sbegin sdone) somesummarizer (ReifiedTransductionM' t0 :< somemachine) somefold =
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

        done (Quartet sstate summarizer (FoldM fstep fstate fdone) _) = do
            (s,bss) <- sdone sstate
            (u,finalfold) <- fdone =<< flip (foldlM fstep) bss =<< fstate
            v <- L.foldM summarizer [u]
            r <- L.foldM finalfold []
            return ((s,v),r)

{-| Summarizes each of the groups demarcated by the 'Transducer' using a
    'Fold'. 
    
    The result value of the 'Transducer' is discarded.

>>> L.fold (folds (chunksOf 3) L.sum L.list) [1..7]
[6,15,7]
-}
folds :: Transducer a b s -> Fold b c -> Transduction a c
folds splitter f = groups splitter (transduce (chokepoint f))

foldsVarying :: Transducer a b s 
             -> Cofree Identity (Fold b c) -- ^ infinite list of 'Fold's.
             -> Transduction a c
foldsVarying splitter foldlist = groupsVarying splitter transducers
    where
    mappytrans :: Fold b c -> ReifiedTransduction' b c ()  
    mappytrans = ReifiedTransduction' . fmap (fmap ((,) ())) . transduce . chokepoint
    transducers = fmap mappytrans foldlist 
    --transducers = fmap (ReifiedTransduction' . _ . transduce . chokepoint) foldlist 

{-| Like 'folds', but preserves the return value of the 'Transducer'.

>>> L.fold (folds' (chunksOf 3) L.sum L.list) [1..7]
((),[6,15,7])
-}
folds' :: Transducer a b s -> Fold b c -> Transduction' a c s
folds' splitter innerfold somefold = 
    fmap (bimap fst id) (groups' splitter L.mconcat innertrans somefold)
    where
    innertrans = fmap ((,) ()) . transduce (chokepoint innerfold)

{-| Monadic version of 'folds'.		

-}
foldsM :: (Applicative m,Monad m) => TransducerM m a b s -> FoldM m b c -> TransductionM m a c
foldsM splitter f = groupsM splitter (transduceM (chokepointM f))


{-| Monadic version of 'folds''.		

-}
foldsM' :: (Applicative m,Monad m) => TransducerM m a b s -> FoldM m b c -> TransductionM' m a c s
foldsM' splitter innerfold somefold = 
    fmap (bimap fst id) (groupsM' splitter (L.generalize L.mconcat) innertrans somefold)
    where
    innertrans = fmap ((,) ()) . transduceM (chokepointM innerfold)

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
        done _ = ((),[])

------------------------------------------------------------------------------

{- $reexports

-}
