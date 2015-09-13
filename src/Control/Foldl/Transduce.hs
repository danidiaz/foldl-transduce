{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

-- |
--
-- This module builds on module "Control.Foldl", adding stateful transducers
-- and grouping operations.

module Control.Foldl.Transduce (
        -- * Transducer types
        Transduction 
    ,   Transduction' 
    ,   Transducer(..)
    ,   ToTransducer(..)
        -- ** Monadic transducer types
    ,   TransductionM
    ,   TransductionM'
    ,   TransducerM(..)
    ,   ToTransducerM(..)
        -- * Applying transducers
    ,   transduce
    ,   transduce'
    ,   transduceM
    ,   transduceM'
    ,   transduceK
        -- * Folding over groups
    ,   folds
    ,   folds'
    ,   foldsM
    ,   foldsM'
        -- * Group operations
    ,   ReifiedTransduction' (..)
    ,   reify
    ,   reify'
    ,   Moore(..)
    ,   ToTransductions' (..)
    ,   moveHead
    ,   groups
    ,   bisect
    ,   groups'
        -- ** Monadic group operations
    ,   ReifiedTransductionM' (..)
    ,   reifyM
    ,   reifyM'
    ,   MooreM(..)
    ,   ToTransductionsM' (..)
    ,   moveHeadM
    ,   groupsM
    ,   bisectM
    ,   groupsM'
        -- * Transducers
    ,   ignore
    ,   surround
    ,   surroundIO
        -- * Splitters
    ,   chunksOf
    ,   splitAt
    ,   chunkedSplitAt
    ,   splitLast
    ,   break
    ,   chunkedStripPrefix
        -- * Transducer utilities
    ,   foldify
    ,   foldifyM
    ,   condense
    ,   condenseM
    ,   hoistTransducer
        -- * Fold utilities
    ,   quiesce
    ,   quiesceWith
    ,   hoistFold
    ,   unit
    ,   trip
    ,   ToFold(..)
    ,   ToFoldM(..)
        -- * Re-exports
        -- $reexports
    ,   module Data.Functor.Extend
    ,   module Control.Foldl
    ,   module Control.Comonad.Cofree
        -- * Deprecated
    ,   splitWhen
    ) where

import Prelude hiding (splitAt,break)

import Data.Bifunctor
import Data.Monoid
import qualified Data.Monoid.Cancellative as CM
import qualified Data.Monoid.Null as NM
import qualified Data.Monoid.Factorial as SFM
import Data.Functor.Identity
import Data.Functor.Extend
import Data.Foldable (Foldable,foldlM,foldl',toList)
import Data.Traversable
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Comonad
import Control.Comonad.Cofree 
import Control.Foldl (Fold(..),FoldM(..))
import qualified Control.Foldl as L
import Control.Foldl.Transduce.Internal (Pair(..),Quartet(..),_1of3)

{- $setup

>>> import qualified Control.Foldl as L
>>> import Control.Foldl.Transduce
>>> import Control.Applicative
>>> import qualified Control.Comonad.Cofree as C
>>> import Prelude hiding (splitAt,break)

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

{-| Helper for storing a 'ReifiedTransduction'' safely on a container.		

-}
newtype ReifiedTransduction' a b r = ReifiedTransduction' { getTransduction' :: Transduction' a b r }

{-| Convenience constructor, often useful with pure functions like 'id'.		

-}
reify :: Transduction a b -> ReifiedTransduction' a b ()
reify t = reify' (fmap (fmap ((,) ())) t)  

reify' :: Transduction' a b r -> ReifiedTransduction' a b r
reify' = ReifiedTransduction' 

{-| A stateful process that transforms a stream of inputs into a stream of
    outputs, and may optionally demarcate groups in the stream of outputs.

    Composed of a step function, an initial state, and a extraction function. 

    The step function returns a triplet of:

    * The new internal state.
    * List of outputs belonging to the last segment detected in the previous step.
    * A list of lists of outputs belonging to segments detected in the current
      step. If the list is empty, that means no splitting has taken place in the
      current step. 'Transducer's that do not perform grouping never return anything
      other than @[]@ here. In effect, they treat the whole stream as a single group.

    The extraction function returns the 'Transducer's own result value, along with any
    pending output.
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

{-| Helps converting monadic transducers (over 'Identity') into pure ones.		

-}
class ToTransducer t where
    toTransducer :: t i o r -> Transducer i o r

instance ToTransducer Transducer where
    toTransducer = id

instance ToTransducer (TransducerM Identity) where
    toTransducer = _simplify

class ToFold t where
    toFold :: t i r -> Fold i r

instance ToFold Fold where
    toFold = id

instance ToFold (FoldM Identity) where
    toFold = L.simplify

{-| Like 'Transduction', but works on monadic 'Fold's.		

-}
type TransductionM m a b = forall x. Monad m => FoldM m b x -> FoldM m a x

{-| Like 'Transduction'', but works on monadic 'Fold's.		

-}
type TransductionM' m a b r = forall x. FoldM m b x -> FoldM m a (r,x)

{-| Helper for storing a 'TransductionM'' safely on a container.		

-}
newtype ReifiedTransductionM' m a b r = ReifiedTransductionM' { getTransductionM' :: TransductionM' m a b r }

{-| Monadic version of 'reify'.		

-}
reifyM :: Monad m => TransductionM m a b -> ReifiedTransductionM' m a b ()
reifyM t = reifyM' (fmap (fmap ((,) ())) t)  

{-| Monadic version of 'reifyM'.		

-}
reifyM' :: TransductionM' m a b r -> ReifiedTransductionM' m a b r
reifyM' = ReifiedTransductionM' 

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


{-| Helps converting pure transducers into monadic ones.		

-}
class ToTransducerM m t where
    toTransducerM :: t i o r -> TransducerM m i o r

-- http://chrisdone.com/posts/haskell-constraint-trick
instance (m ~ m') => ToTransducerM m (TransducerM m') where
    toTransducerM = id

instance Monad m => ToTransducerM m Transducer where
    toTransducerM = _generalize

class ToFoldM m t where
    toFoldM :: t i r -> FoldM m i r

instance (m ~ m') => ToFoldM m (FoldM m') where
    toFoldM = id

instance Monad m => ToFoldM m Fold where
    toFoldM = L.generalize

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

{-| Transduce with a Kleisli arrow that returns a list.		

-}
transduceK :: (Monad m) => (i -> m [o]) -> TransductionM m i o 
transduceK k = transduceM (TransducerM step (return ()) (\_ -> return ((),[],[])))
    where
    step _ i = liftM (\os -> ((),os,[])) (k i)


------------------------------------------------------------------------------

{-| Ignore all the inputs coming into the fold.

    Polymorphic in both inputs and outputs.		

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

    Used as a splitter, it puts the prefix, the original stream and
    the suffix in separate groups:

>>> L.fold (groups (surround "prefix" "suffix") (surround "[" "]") L.list) "middle"
"[prefix][middle][suffix]"

-}
surround :: (Traversable p, Traversable s) => p a -> s a -> Transducer a a ()
surround (toList -> ps) (toList -> ss) = 
    Transducer step PrefixPending done 
    where
        step PrefixPending a = 
            (PrefixAdded, ps,[[a]])
        step PrefixAdded a = 
            (PrefixAdded, [a],[])
        done PrefixPending = 
            ((), ps, [[],ss])
        done PrefixAdded = 
            ((), [], [ss])

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
            return (PrefixAdded, ps, [[a]])
        step PrefixAdded a = 
            return (PrefixAdded, [a], [])
        done PrefixPending = do
            ps <- fmap toList prefixa
            ss <- fmap toList suffixa
            return ((), ps, [[],ss])
        done PrefixAdded = do
            ss <- fmap toList suffixa
            return ((), [], [ss])

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

{-| Monadic version of 'condense'.		

-}
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

{-| Turn a 'FoldM' that fails abruptly into one that encodes the error into
    its return value.

    Can be useful when combining fallible 'FoldM's with non-fallible ones.

>>> L.foldM (quiesce (FoldM (\_ _-> throwE ()) (return ()) (\_ -> throwE ()))) [1..7]
Left ()
-}
quiesce :: Monad m => FoldM (ExceptT e m) a r -> FoldM m a (Either e r)
quiesce (FoldM step initial done) = 
    FoldM step' (runExceptT initial) done'
    where
    step' x i = do  
        case x of
            Left _ -> return x
            Right notyetfail -> runExceptT (step notyetfail i)
    done' x = do
        case x of 
            Left e -> return (Left e)
            Right notyetfail -> do
                result <- runExceptT (done notyetfail)
                case result of 
                    Left e -> return (Left e)
                    Right r -> return (Right r)

{-| Generalized version of 'quiesce' to turn a fallible 'FoldM' into another
    that starts a "fallback fold" when it encounters an error.

    "Start folding this way, if you encounter an error, start folding this 
    other way".                               

>>> L.foldM (quiesceWith (L.generalize L.length) (FoldM (\_ _-> throwE ()) (return ()) (\_ -> throwE ()))) [1..7]
Left ((),7)
-}
quiesceWith :: (Functor m,Monad m) => FoldM m a v -> FoldM (ExceptT e m) a r -> FoldM m a (Either (e,v) r)
quiesceWith fallbackFold (FoldM step initial done) = 
    FoldM step' (runExceptT (withExceptT (Pair fallbackFold) initial)) done'
    where
    step' x i = do  
        case x of
            Left (Pair ffold e) -> do
                ffold' <- L.foldM (duplicated ffold) [i]
                return (Left (Pair ffold' e))
            Right notyetfail -> do
                 x' <- runExceptT (step notyetfail i)
                 case x' of
                     Left e -> do
                         ffold <- L.foldM (duplicated fallbackFold) [i]
                         return (Left (Pair ffold e))
                     Right x'' -> return (Right x'')
    done' x = case x of
            Left (Pair ffold e) -> do
                alternativeResult <- L.foldM ffold []
                return (Left (e,alternativeResult))
            Right notyetfail -> do 
                x' <- runExceptT (done notyetfail)
                case x' of
                    Left e -> do
                        alternativeResult <- L.foldM fallbackFold []
                        return (Left (e,alternativeResult))
                    Right x'' -> return (Right x'')

{-| The "do-nothing" fold.		

-}
unit :: Fold a ()
unit = pure () 

{-| A fold that fails if it receives any input at all. The received input is
    used as the error.		

-}
trip :: Monad m => FoldM (ExceptT a m) a ()
trip = FoldM (\_ x -> throwE x) (return ()) (\_ -> return mempty)

------------------------------------------------------------------------------

{-| An unending machine that eats @u@ values and returns 
    'ReifiedTransduction''s whose result type is also @u@.

-}
newtype Moore a b u = Moore { getMoore :: Cofree ((->) u) (ReifiedTransduction' a b u) }

{-| Monadic version of 'Moore'.		

-}
newtype MooreM m a b u = MooreM { getMooreM :: Cofree ((->) u) (ReifiedTransductionM' m a b u) }

{-| Prepend the head of the first argument to the second argument.		

-}
moveHead :: (ToTransductions' h,ToTransductions' t) => h a b u -> t a b u -> Moore a b u 
moveHead (toTransductions' -> Moore (theHead :< _)) (toTransductions' -> Moore theTail) = Moore (theHead :< const theTail)

{-| Monadic version of 'moveHead'.		

-}
moveHeadM :: (Monad m, ToTransductionsM' m h, ToTransductionsM' m t) => h a b u -> t a b u -> MooreM m a b u 
moveHeadM (toTransductionsM' -> MooreM (theHead :< _)) (toTransductionsM' -> MooreM theTail) = MooreM (theHead :< const theTail)

{-| Helper for obtaining infinite sequences of 'Transduction''s from suitable
    types (in order to avoid explicit conversions).		

-}
class ToTransductions' t where
    toTransductions' :: t a b u -> Moore a b u

instance ToTransductions' Moore where
    toTransductions' = id

instance ToTransductions' Transducer where
    toTransductions' t = toTransductions' (reify' (transduce' t))

instance ToTransductions' ReifiedTransduction' where
    toTransductions' = Moore . coiter const

{-| Monadic version of 'ToTransductions''.		

-}
class Monad m => ToTransductionsM' m t where
    toTransductionsM' :: t a b u -> MooreM m a b u

instance (m ~ m', Monad m') => ToTransductionsM' m (MooreM m') where
    toTransductionsM' = id

instance (m ~ m', Monad m') => ToTransductionsM' m (TransducerM m') where
    toTransductionsM' t = toTransductionsM' (reifyM' (transduceM' t))

instance Monad m => ToTransductionsM' m Transducer where
    toTransductionsM' (toTransducerM -> t) = toTransductionsM' (reifyM' (transduceM' t))

instance (m ~ m', Monad m') => ToTransductionsM' m (ReifiedTransductionM' m') where
    toTransductionsM' = MooreM . coiter const

{-| Processes each of the groups demarcated by a 'Transducer' using 
    a 'Transduction' taken from an unending supply, 
    returning a 'Transduction' what works over the undivided stream of inputs. 
    
    The return value of the 'Transducer' is discarded.

>>> L.fold (groups (chunksOf 2) (surround "<" ">") L.list) "aabbccdd"
"<aa><bb><cc><dd>"

>>> :{ 
    let transductions = Moore (C.unfold (\i ->
          (reify (transduce (surround (show i) [])), \_ -> succ i)) 0)
    in L.fold (groups (chunksOf 2) transductions L.list) "aabbccdd"
    :}
"0aa1bb2cc3dd"
-}
groups :: (ToTransducer s, ToTransductions' t) 
       => s a b r  -- ^ 'Transducer' working as a splitter.
       -> t b c () -- ^ infinite list of transductions
       -> Transduction a c 
groups splitter transductions oldfold = 
        fmap snd (groups' splitter transductions unit oldfold)

{-| Use a different 'Transduction' for the first detected group.		

>>> :{ 
    let drop n = bisect (splitAt n) ignore (reify id)
    in L.fold (drop 2 L.list) "aabbccdd"
    :}
"bbccdd"
-}
bisect :: (ToTransducer s, ToTransductions' h, ToTransductions' t)
       => s a b r -- ^ 'Transducer' working as a splitter.
       -> h b c () -- ^ Machine to process the first group
       -> t b c () -- ^ Machine to process the second and subsequent groups
       -> Transduction a c
bisect sp t1 t2 = groups sp (moveHead t1 t2)

{-| Generalized version of 'groups' that preserves the return value of the
    'Transducer'.

    A summary value for each group is also calculated. These values are 
    aggregated for the whole stream, with the help of an auxiliary 'Fold'.


>>> :{ 
    let transductions = 
          reify' (\f -> transduce (surround "<" ">") ((,) <$> L.list <*> f))
    in L.fold (groups' (chunksOf 2) transductions L.list L.list) "aabbccdd"
    :}
(((),["<aa>","<bb>","<cc>","<dd>"]),"<aa><bb><cc><dd>")
-}
groups' :: (ToTransducer s, ToTransductions' t, ToFold f)
        => s a b r -- ^ 'Transducer' working as a splitter. 
        -> t b c u -- ^ machine that eats @u@ values and spits transductions
        -> f     u v -- ^ auxiliary 'Fold' that aggregates the @u@ values produced for each group
        -> Transduction' a c (r,v) 
groups' (toTransducer -> Transducer sstep sbegin sdone) 
        (toTransductions' -> Moore (ReifiedTransduction' t0 :< somemachine)) 
        (toFold -> Fold astep abegin adone) 
        somefold 
        =
    Fold step (Quartet sbegin somemachine abegin (t0 (duplicated somefold))) done 
      where 
        step (Quartet sstate machine astate innerfold) i =
           let 
               (sstate',oldSplit,newSplits) = sstep sstate i
               (machine',astate',innerfold') = 
                   foldl' 
                   step'
                   (machine,astate,feed innerfold oldSplit) 
                   newSplits
           in
           Quartet sstate' machine' astate' innerfold' 
        
        done (Quartet sstate machine astate innerfold) = 
            let 
                (s,oldSplit,newSplits) = sdone sstate
                (_,astate',innerfold') = 
                   foldl' 
                   step'
                   (machine,astate,feed innerfold oldSplit) 
                   newSplits
                (u,finalfold) = extract innerfold'
            in  ((s,adone (astep astate' u)),extract finalfold)

        step' (machine_,astate,innerfold_) somesplit = 
           let (u,resetted,nextmachine) = reset machine_ innerfold_
           in  (nextmachine,astep astate u,feed resetted somesplit)

        feed = L.fold . duplicated

        reset machine (Fold _ fstate fdone) = 
            let (u,nextfold) = fdone fstate
                ReifiedTransduction' t1 :< nextmachine = machine u
            in  (u,t1 (duplicated nextfold),nextmachine)


{-| Monadic version of 'groups'.		

-}
groupsM :: (Monad m, ToTransducerM m s, ToTransductionsM' m t)
               => s a b r -- ^
               -> t b c ()
               -> TransductionM m a c
groupsM splitter transductions oldfold = 
        fmap snd (groupsM' splitter transductions unit oldfold)


{-| Monadic version of 'bisect'.		

-}
bisectM :: (Monad m, ToTransducerM m s, ToTransductionsM' m h, ToTransductionsM' m t)
               => s a b r -- ^
               -> h b c ()
               -> t b c ()
               -> TransductionM m a c
bisectM s t1 t2 = groupsM s (moveHeadM t1 t2)

{-| Monadic version of 'groups''.		

-}
groupsM' :: (Monad m, ToTransducerM m s, ToTransductionsM' m t, ToFoldM m f) 
         => s a b r 
         -> t b c u -- ^ 
         -> f     u v 
         -> TransductionM' m a c (r,v) 
groupsM' (toTransducerM -> TransducerM sstep sbegin sdone) 
         (toTransductionsM' -> MooreM (ReifiedTransductionM' t0 :< somemachine)) 
         (toFoldM -> FoldM astep abegin adone) 
         somefold 
         =
    FoldM step 
          (do sbegin' <- sbegin
              abegin' <- abegin
              return (Quartet sbegin' somemachine abegin' (t0 (duplicated somefold))))
          done        
    where
        step (Quartet sstate machine astate innerfold) i = do
            (sstate',oldSplit, newSplits) <- sstep sstate i 
            innerfold' <- feed innerfold oldSplit
            (machine',astate',innerfold'') <- foldlM step' (machine,astate,innerfold') newSplits
            return $! Quartet sstate' machine' astate' innerfold'' 

        done (Quartet sstate machine astate innerfold) = do
            (s,oldSplit,newSplits) <- sdone sstate
            innerfold' <- feed innerfold oldSplit
            (_,astate',innerfold'') <- foldlM step' (machine,astate,innerfold') newSplits
            (u,finalfold) <- L.foldM innerfold'' []
            v <- adone =<< astep astate' u
            r <- L.foldM finalfold []
            return ((s,v),r)

        step' (machine,astate,innerfold) is = do
            (u,innerfold',machine') <- reset machine innerfold 
            astate' <- astep astate u
            innerfold'' <- feed innerfold' is
            return $! (machine',astate',innerfold'') 

        feed = L.foldM . duplicated

        reset machine (FoldM _ fstate fdone) = do
           (u,nextfold) <- fdone =<< fstate 
           let 
               ReifiedTransductionM' t1 :< nextmachine = machine u
           return (u,t1 (duplicated nextfold),nextmachine)

{-| Summarizes each of the groups demarcated by the 'Transducer' using a
    'Fold'. 
    
    The result value of the 'Transducer' is discarded.

>>> L.fold (folds (chunksOf 3) L.sum L.list) [1..7]
[6,15,7]
-}
folds :: (ToTransducer t, ToFold f) 
      => t a b s -- ^ 'Transducer' working as a splitter.
      -> f b c 
      -> Transduction a c
folds splitter (toFold -> f) = groups splitter (fmap (const ()) (condense f))

{-| Like 'folds', but preserves the return value of the 'Transducer'.

>>> L.fold (folds' (chunksOf 3) L.sum L.list) [1..7]
((),[6,15,7])
-}
folds' :: (ToTransducer t, ToFold f) 
       => t a b s -- ^ 'Transducer' working as a splitter.
       -> f b c 
       -> Transduction' a c s
folds' splitter (toFold -> innerfold) somefold = 
    fmap (bimap fst id) (groups' splitter innertrans unit somefold)
    where
    innertrans = reify' $ \x -> fmap ((,) ()) (transduce (condense innerfold) x)

{-| Monadic version of 'folds'.		

-}
foldsM :: (Applicative m, Monad m, ToTransducerM m t, ToFoldM m f) 
       => t a b s -- ^
       -> f b c 
       -> TransductionM m a c
foldsM splitter (toFoldM -> f) = groupsM splitter (fmap (const ()) (condenseM f))

{-| Monadic version of 'folds''.		

-}
foldsM' :: (Applicative m,Monad m, ToTransducerM m t, ToFoldM m f) 
        => t a b s -- ^
        -> f b c 
        -> TransductionM' m a c s
foldsM' splitter (toFoldM -> innerfold) somefold = 
    fmap (bimap fst id) (groupsM' splitter innertrans unit somefold)
    where
    innertrans = reifyM' $ \x -> fmap ((,) ()) (transduceM (condenseM innerfold) x)

------------------------------------------------------------------------------

{-| Splits a stream into chunks of fixed size.		

>>> L.fold (folds (chunksOf 2) L.list L.list) [1..7]
[[1,2],[3,4],[5,6],[7]]

>>> L.fold (groups (chunksOf 2) (surround [] [0]) L.list) [1..7]
[1,2,0,3,4,0,5,6,0,7,0]
-}
chunksOf :: Int -> Transducer a a ()
chunksOf 0 = Transducer (\_ _ -> ((),[],repeat [])) () (error "never happens")
chunksOf groupSize = Transducer step groupSize done 
    where
        step 0 a = (pred groupSize, [], [[a]])
        step i a = (pred i, [a], [])
        done _ = ((),[],[])

{-| Splits the stream at a given position.		

>>> L.fold (bisect (splitAt 2) ignore (reify id) L.list) [1..5]
[3,4,5]

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

{-| Similar to `splitAt`, but works with streams of "chunked" data like
    bytestrings, texts, vectors, lists of lists...		

>>> L.fold (bisect (chunkedSplitAt 7) ignore (reify id) L.list) [[1..5],[6..9]]
[[8,9]]

-}
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

>>> L.fold (bisect (break (>3)) (reify id) ignore L.list) [1..5]
[1,2,3]
-}
break :: (a -> Bool) -> Transducer a a ()
break predicate = 
    Transducer step SplitWhenConditionPending done 
    where
        step SplitWhenConditionPending i = 
            if predicate i 
               then (SplitWhenConditionEncountered,[],[[i]])
               else (SplitWhenConditionPending,[i],[])
        step SplitWhenConditionEncountered i = 
               (SplitWhenConditionEncountered,[i],[])
        done = mempty

{-| Puts the last element of the input stream (if it exists) in a separate
    group.

>>> L.fold (bisect splitLast (reify id) ignore L.list) [1..5]
[1,2,3,4]
-}
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

{-| Strip a prefix from a stream of "chunked" data, like packed text.		

    If the prefix doesn't match, fail with the unmatched part of the prefix and
    the input that caused the error.

>>> runExceptT $ L.foldM (transduceM (chunkedStripPrefix [[1..2],[3..4]]) (L.generalize L.list)) [[1..5],[6..9]]
Right [[5],[6,7,8,9]]

>>> runExceptT $ L.foldM (transduceM (chunkedStripPrefix [[1..2],[3,77,99]]) (L.generalize L.list)) [[1..5],[6..9]]
Left ([[77,99]],Just [4,5])
-}
chunkedStripPrefix :: (CM.LeftGCDMonoid i,SFM.StableFactorialMonoid i,Traversable t,Monad m) 
                   => t i -- ^
                   -> TransducerM (ExceptT ([i],Maybe i) m) i i ()
chunkedStripPrefix (filter (not . NM.null) . toList -> chunks) = 
    TransducerM step (return chunks) done
    where
        step []     i = 
            return ([],[i],[])
        step (x:xs) i = 
            let (prefix',i',x') = CM.stripCommonPrefix i x 
            in 
            if NM.null prefix'
                then throwE (x:xs,Just i)
                else 
                    if NM.null x' 
                       then step xs i'
                       else step (x':xs) i'
        done [] = 
            return mempty
        done (x:xs) = 
            throwE (x:xs, Nothing) 

------------------------------------------------------------------------------

{- $reexports

-}


{-# DEPRECATED splitWhen "use break instead" #-}
splitWhen :: (a -> Bool) -> Transducer a a ()
splitWhen = break
