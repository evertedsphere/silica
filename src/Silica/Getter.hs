{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

#if __GLASGOW_HASKELL__ < 708
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE NoPolyKinds #-}
{-# LANGUAGE NoDataKinds #-}
#endif

-- Disable the warnings generated by 'to', 'ito', 'like', 'ilike'.
-- These functions are intended to produce 'R_Getters'. Without this constraint
-- users would get warnings when annotating types at uses of these functions.
#if __GLASGOW_HASKELL__ >= 711
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Silica.Getter
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
--
-- A @'R_Getter' s a@ is just any function @(s -> a)@, which we've flipped
-- into continuation passing style, @(a -> r) -> s -> r@ and decorated
-- with 'Const' to obtain:
--
-- @type 'Getting' r s a = (a -> 'Const' r a) -> s -> 'Const' r s@
--
-- If we restrict access to knowledge about the type 'r', we could get:
--
-- @type 'R_Getter' s a = forall r. 'Getting' r s a@
--
-- However, for 'R_Getter' (but not for 'Getting') we actually permit any
-- functor @f@ which is an instance of both 'Functor' and 'Contravariant':
--
-- @type 'R_Getter' s a = forall f. ('Contravariant' f, 'Functor' f) => (a -> f a) -> s -> f s@
--
-- Everything you can do with a function, you can do with a 'R_Getter', but
-- note that because of the continuation passing style ('.') composes them
-- in the opposite order.
--
-- Since it is only a function, every 'R_Getter' obviously only retrieves a
-- single value for a given input.
--
-- A common question is whether you can combine multiple 'R_Getter's to
-- retrieve multiple values. Recall that all 'R_Getter's are 'R_Fold's and that
-- we have a @'Monoid' m => 'Applicative' ('Const' m)@ instance to play
-- with. Knowing this, we can use @'Data.Monoid.<>'@ to glue 'R_Fold's
-- together:
--
-- >>> import Data.Monoid
-- >>> (1, 2, 3, 4, 5) ^.. (_2 <> _3 <> _5)
-- [2,3,5]
--
-------------------------------------------------------------------------------
module Silica.Getter
  (
  -- * R_Getters
    R_Getter, R_IndexedGetter
  , Getting, IndexedGetting
  , Accessing
  -- * Building R_Getters
  , to
  , uto
  , xto
  , ito
  , like
  , ilike
  -- * Combinators for R_Getters and R_Folds
  , (^.)
  , view, views
  , use, uses
  , listening, listenings
  -- * Indexed R_Getters
  -- ** Indexed R_Getter Combinators
  , (^@.)
  , iview, iviews
  , iuse, iuses
  , ilistening, ilistenings
  -- * Implementation Details
  , Contravariant(..)
  , pgetting
  , Const(..)
  ) where

import Control.Applicative
import Silica.Internal.Indexed
import Silica.Type
import Control.Monad.Reader.Class as Reader
import Control.Monad.State        as State
import Control.Monad.Writer       as Writer
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Profunctor.Unsafe

-- $setup
-- >>> :set -XNoR_OverloadedStrings
-- >>> import Silica
-- >>> import Data.List.Lens
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g

infixl 8 ^., ^@.

asGetter :: AsGetter k => Optic' k s a -> Getter s a
asGetter = sub

asGetting :: AsGetting r k => Optic' k s a -> Getting r s a
asGetting = sub

asIndexedGetter :: AsIndexedGetter i k => Optic' k s a -> IndexedGetter i s a
asIndexedGetter = sub

asIndexedGetting :: AsIndexedGetting i r k => Optic' k s a -> IndexedGetting i r s a
asIndexedGetting = sub

runGetter :: AsGetter k => Optic' k s a -> R_Getter s a
runGetter = runOptic . asGetter

runGetting :: AsGetting r k => Optic' k s a -> R_Getting r s a
runGetting = runOptic . asGetting

-- runIndexedGetter :: AsIndexedGetter i k => Optic' k s a -> R_IndexedGetter i s a
-- runIndexedGetter = runOptic . asIndexedGetter

-------------------------------------------------------------------------------
-- R_Getters
-------------------------------------------------------------------------------

-- | Build an (index-preserving) 'R_Getter' from an arbitrary Haskell function.
--
-- @
-- 'to' f '.' 'to' g ≡ 'to' (g '.' f)
-- @
--
-- @
-- a '^.' 'to' f ≡ f a
-- @
--
-- >>> a ^.to f
-- f a
--
-- >>> ("hello","world")^.to snd
-- "world"
--
-- >>> 5^.to succ
-- 6
--
-- >>> (0, -5)^._2.to abs
-- 5
--
-- @
-- 'to' :: (s -> a) -> 'R_IndexPreservingGetter' s a
-- @
to :: (s -> a) -> IndexPreservingGetter s a
to = xto
{-# INLINE to #-}

-- | Index-preserving 'to', for consistency
xto :: (s -> a) -> IndexPreservingGetter s a
xto k = Optic (rto k)
{-# INLINE xto #-}

-- | Unindexed 'to'
uto :: (s -> a) -> Getter s a
uto k = Optic (rto k)
{-# INLINE uto #-}

-- | General 'to'.
gto :: (Profunctor p, Contravariant f) => (s -> a) -> Proptic' p f s a
gto k = Optic (rto k)
{-# INLINE gto #-}

-- | Raw 'to'
rto :: (Profunctor p, Contravariant f) => (s -> a) -> R_Optic' p f s a
rto k = dimap k (contramap k)
{-# INLINE rto #-}

-- pto :: (Profunctor p, Contravariant f, AsProptic p f k) => (s -> a) -> Optic' k s a
-- pto k = Optic (rto k)
-- {-# INLINE pto #-}

ito :: (s -> (i, a)) -> IndexedGetter i s a
ito k = Optic (rito k)
{-# INLINE ito #-}

gito :: (Indexable i p, Contravariant f) => (s -> (i, a)) -> Over' p f s a
gito k = Optic (rito k)
{-# INLINE gito #-}

rito :: (Indexable i p, Contravariant f) => (s -> (i, a)) -> R_Over' p f s a
rito k = dimap k (contramap (snd . k)) . uncurry . indexed
{-# INLINE rito #-}

-- | Build an constant-valued (index-preserving) 'R_Getter' from an arbitrary Haskell value.
--
-- @
-- 'like' a '.' 'like' b ≡ 'like' b
-- a '^.' 'like' b ≡ b
-- a '^.' 'like' b ≡ a '^.' 'to' ('const' b)
-- @
--
-- This can be useful as a second case 'failing' a 'R_Fold'
-- e.g. @foo `failing` 'like' 0@
--
-- @
-- 'like' :: a -> 'R_IndexPreservingGetter' s a
-- like :: (Profunctor p, Contravariant f, Functor f) => a -> R_Optic' p f s a
-- @
like :: a -> IndexPreservingGetter s a
like = xlike
{-# INLINE like #-}

xlike :: a -> IndexPreservingGetter s a
xlike a = xto (const a)
{-# INLINE xlike #-}

ulike :: a -> Getter s a
ulike a = uto (const a)
{-# INLINE ulike #-}

glike :: (Profunctor p, Contravariant f) => a -> Proptic' p f s a
glike a = gto (const a)
{-# INLINE glike #-}

ilike :: i -> a -> IndexedGetter i s a
ilike i a = ito (const (i, a))
{-# INLINE ilike #-}

gilike :: (Indexable i p, Contravariant f) => i -> a -> Over' p f s a
gilike i a = gito (const (i, a))
{-# INLINE gilike #-}

-- | When you see this in a type signature it indicates that you can
-- pass the function a 'R_Lens', 'R_Getter',
-- 'Control.R_Lens.R_Traversal.R_Traversal', 'Control.R_Lens.R_Fold.R_Fold',
-- 'Control.R_Lens.R_Prism.R_Prism', 'Control.R_Lens.R_Iso.R_Iso', or one of
-- the indexed variants, and it will just \"do the right thing\".
--
-- Most 'R_Getter' combinators are able to be used with both a 'R_Getter' or a
-- 'Control.R_Lens.R_Fold.R_Fold' in limited situations, to do so, they need to be
-- monomorphic in what we are going to extract with 'Control.Applicative.Const'. To be compatible
-- with 'R_Lens', 'Control.R_Lens.R_Traversal.R_Traversal' and
-- 'Control.R_Lens.R_Iso.R_Iso' we also restricted choices of the irrelevant @t@ and
-- @b@ parameters.
--
-- If a function accepts a @'Getting' r s a@, then when @r@ is a 'Data.Monoid.Monoid', then
-- you can pass a 'Control.R_Lens.R_Fold.R_Fold' (or
-- 'Control.R_Lens.R_Traversal.R_Traversal'), otherwise you can only pass this a
-- 'R_Getter' or 'R_Lens'.
type R_Getting r s a = (a -> Const r a) -> s -> Const r s

-- | Used to consume an 'Control.R_Lens.R_Fold.R_IndexedFold'.
type R_IndexedGetting i m s a = Indexed i a (Const m a) -> s -> Const m s

-- | This is a convenient alias used when consuming (indexed) getters and (indexed) folds
-- in a highly general fashion.
type Accessing p m s a = p a (Const m a) -> s -> Const m s

-------------------------------------------------------------------------------
-- Getting Values
-------------------------------------------------------------------------------

-- | View the value pointed to by a 'R_Getter', 'Control.R_Lens.R_Iso.R_Iso' or
-- 'R_Lens' or the result of folding over all the results of a
-- 'Control.R_Lens.R_Fold.R_Fold' or 'Control.R_Lens.R_Traversal.R_Traversal' that points
-- at a monoidal value.
--
-- @
-- 'view' '.' 'to' ≡ 'id'
-- @
--
-- >>> view (to f) a
-- f a
--
-- >>> view _2 (1,"hello")
-- "hello"
--
-- >>> view (to succ) 5
-- 6
--
-- >>> view (_2._1) ("hello",("world","!!!"))
-- "world"
--
--
-- R_As 'view' is commonly used to access the target of a 'R_Getter' or obtain a monoidal summary of the targets of a 'R_Fold',
-- It may be useful to think of it as having one of these more restricted signatures:
--
-- @
-- 'view' ::             'R_Getter' s a     -> s -> a
-- 'view' :: 'Data.Monoid.Monoid' m => 'Control.R_Lens.R_Fold.R_Fold' s m       -> s -> m
-- 'view' ::             'Control.R_Lens.R_Iso.R_Iso'' s a       -> s -> a
-- 'view' ::             'R_Lens'' s a      -> s -> a
-- 'view' :: 'Data.Monoid.Monoid' m => 'Control.R_Lens.R_Traversal.R_Traversal'' s m -> s -> m
-- @
--
-- In a more general setting, such as when working with a 'Monad' transformer stack you can use:
--
-- @
-- 'view' :: 'MonadReader' s m             => 'R_Getter' s a     -> m a
-- 'view' :: ('MonadReader' s m, 'Data.Monoid.Monoid' a) => 'Control.R_Lens.R_Fold.R_Fold' s a       -> m a
-- 'view' :: 'MonadReader' s m             => 'Control.R_Lens.R_Iso.R_Iso'' s a       -> m a
-- 'view' :: 'MonadReader' s m             => 'R_Lens'' s a      -> m a
-- 'view' :: ('MonadReader' s m, 'Data.Monoid.Monoid' a) => 'Control.R_Lens.R_Traversal.R_Traversal'' s a -> m a
-- @
view :: (MonadReader s m, AsGetting a k) => Optic' k s a -> m a
view l = Reader.asks (getConst #. runGetting l Const)
{-# INLINE view #-}

-- | View a function of the value pointed to by a 'R_Getter' or 'R_Lens' or the result of
-- folding over the result of mapping the targets of a 'Control.R_Lens.R_Fold.R_Fold' or
-- 'Control.R_Lens.R_Traversal.R_Traversal'.
--
-- @
-- 'views' l f ≡ 'view' (l '.' 'to' f)
-- @
--
-- >>> views (to f) g a
-- g (f a)
--
-- >>> views _2 length (1,"hello")
-- 5
--
-- R_As 'views' is commonly used to access the target of a 'R_Getter' or obtain a monoidal summary of the targets of a 'R_Fold',
-- It may be useful to think of it as having one of these more restricted signatures:
--
-- @
-- 'views' ::             'R_Getter' s a     -> (a -> r) -> s -> r
-- 'views' :: 'Data.Monoid.Monoid' m => 'Control.R_Lens.R_Fold.R_Fold' s a       -> (a -> m) -> s -> m
-- 'views' ::             'Control.R_Lens.R_Iso.R_Iso'' s a       -> (a -> r) -> s -> r
-- 'views' ::             'R_Lens'' s a      -> (a -> r) -> s -> r
-- 'views' :: 'Data.Monoid.Monoid' m => 'Control.R_Lens.R_Traversal.R_Traversal'' s a -> (a -> m) -> s -> m
-- @
--
-- In a more general setting, such as when working with a 'Monad' transformer stack you can use:
--
-- @
-- 'views' :: 'MonadReader' s m             => 'R_Getter' s a     -> (a -> r) -> m r
-- 'views' :: ('MonadReader' s m, 'Data.Monoid.Monoid' r) => 'Control.R_Lens.R_Fold.R_Fold' s a       -> (a -> r) -> m r
-- 'views' :: 'MonadReader' s m             => 'Control.R_Lens.R_Iso.R_Iso'' s a       -> (a -> r) -> m r
-- 'views' :: 'MonadReader' s m             => 'R_Lens'' s a      -> (a -> r) -> m r
-- 'views' :: ('MonadReader' s m, 'Data.Monoid.Monoid' r) => 'Control.R_Lens.R_Traversal.R_Traversal'' s a -> (a -> r) -> m r
-- @
--
-- @
-- 'views' :: 'MonadReader' s m => 'Getting' r s a -> (a -> r) -> m r
-- @
views :: (MonadReader s m, AsLensLike (Const r) k) => Optic' k s a -> (a -> r) -> m r
views l f = Reader.asks (getConst #. runLensLike l (Const #. f))
{-# INLINE views #-}

-- | View the value pointed to by a 'R_Getter' or 'R_Lens' or the
-- result of folding over all the results of a 'Control.R_Lens.R_Fold.R_Fold' or
-- 'Control.R_Lens.R_Traversal.R_Traversal' that points at a monoidal values.
--
-- This is the same operation as 'view' with the arguments flipped.
--
-- The fixity and semantics are such that subsequent field accesses can be
-- performed with ('Prelude..').
--
-- >>> (a,b)^._2
-- b
--
-- >>> ("hello","world")^._2
-- "world"
--
-- >>> import Data.Complex
-- >>> ((0, 1 :+ 2), 3)^._1._2.to magnitude
-- 2.23606797749979
--
-- @
-- ('^.') ::             s -> 'R_Getter' s a     -> a
-- ('^.') :: 'Data.Monoid.Monoid' m => s -> 'Control.R_Lens.R_Fold.R_Fold' s m       -> m
-- ('^.') ::             s -> 'Control.R_Lens.R_Iso.R_Iso'' s a       -> a
-- ('^.') ::             s -> 'R_Lens'' s a      -> a
-- ('^.') :: 'Data.Monoid.Monoid' m => s -> 'Control.R_Lens.R_Traversal.R_Traversal'' s m -> m
-- @
(^.) :: AsGetting a k => s -> Optic' k s a -> a
s ^. l = getConst (runGetting l Const s)
{-# INLINE (^.) #-}

-------------------------------------------------------------------------------
-- MonadState
-------------------------------------------------------------------------------

-- | Use the target of a 'R_Lens', 'Control.R_Lens.R_Iso.R_Iso', or
-- 'R_Getter' in the current state, or use a summary of a
-- 'Control.R_Lens.R_Fold.R_Fold' or 'Control.R_Lens.R_Traversal.R_Traversal' that points
-- to a monoidal value.
--
-- >>> evalState (use _1) (a,b)
-- a
--
-- >>> evalState (use _1) ("hello","world")
-- "hello"
--
-- @
-- 'use' :: 'MonadState' s m             => 'R_Getter' s a     -> m a
-- 'use' :: ('MonadState' s m, 'Data.Monoid.Monoid' r) => 'Control.R_Lens.R_Fold.R_Fold' s r       -> m r
-- 'use' :: 'MonadState' s m             => 'Control.R_Lens.R_Iso.R_Iso'' s a       -> m a
-- 'use' :: 'MonadState' s m             => 'R_Lens'' s a      -> m a
-- 'use' :: ('MonadState' s m, 'Data.Monoid.Monoid' r) => 'Control.R_Lens.R_Traversal.R_Traversal'' s r -> m r
-- @
use :: (MonadState s m, AsGetting a k) => Optic' k s a -> m a
use l = State.gets (view l)
{-# INLINE use #-}

-- | Use the target of a 'R_Lens', 'Control.R_Lens.R_Iso.R_Iso' or
-- 'R_Getter' in the current state, or use a summary of a
-- 'Control.R_Lens.R_Fold.R_Fold' or 'Control.R_Lens.R_Traversal.R_Traversal' that
-- points to a monoidal value.
--
-- >>> evalState (uses _1 length) ("hello","world")
-- 5
--
-- @
-- 'uses' :: 'MonadState' s m             => 'R_Getter' s a     -> (a -> r) -> m r
-- 'uses' :: ('MonadState' s m, 'Data.Monoid.Monoid' r) => 'Control.R_Lens.R_Fold.R_Fold' s a       -> (a -> r) -> m r
-- 'uses' :: 'MonadState' s m             => 'R_Lens'' s a      -> (a -> r) -> m r
-- 'uses' :: 'MonadState' s m             => 'Control.R_Lens.R_Iso.R_Iso'' s a       -> (a -> r) -> m r
-- 'uses' :: ('MonadState' s m, 'Data.Monoid.Monoid' r) => 'Control.R_Lens.R_Traversal.R_Traversal'' s a -> (a -> r) -> m r
-- @
--
-- @
-- 'uses' :: 'MonadState' s m => 'Getting' r s t a b -> (a -> r) -> m r
-- @
uses :: (MonadState s m, AsLensLike (Const r) k) => Optic' k s a -> (a -> r) -> m r
uses l f = State.gets (views l f)
{-# INLINE uses #-}

-- | This is a generalized form of 'listen' that only extracts the portion of
-- the log that is focused on by a 'R_Getter'. If given a 'R_Fold' or a 'R_Traversal'
-- then a monoidal summary of the parts of the log that are visited will be
-- returned.
--
-- @
-- 'listening' :: 'MonadWriter' w m             => 'R_Getter' w u     -> m a -> m (a, u)
-- 'listening' :: 'MonadWriter' w m             => 'R_Lens'' w u      -> m a -> m (a, u)
-- 'listening' :: 'MonadWriter' w m             => 'R_Iso'' w u       -> m a -> m (a, u)
-- 'listening' :: ('MonadWriter' w m, 'Monoid' u) => 'R_Fold' w u       -> m a -> m (a, u)
-- 'listening' :: ('MonadWriter' w m, 'Monoid' u) => 'R_Traversal'' w u -> m a -> m (a, u)
-- 'listening' :: ('MonadWriter' w m, 'Monoid' u) => 'R_Prism'' w u     -> m a -> m (a, u)
-- @
listening :: (MonadWriter w m, AsGetting u k) => Optic' k w u -> m a -> m (a, u)
listening l m = do
  (a, w) <- listen m
  return (a, view l w)
{-# INLINE listening #-}

-- | This is a generalized form of 'listen' that only extracts the portion of
-- the log that is focused on by a 'R_Getter'. If given a 'R_Fold' or a 'R_Traversal'
-- then a monoidal summary of the parts of the log that are visited will be
-- returned.
--
-- @
-- 'ilistening' :: 'MonadWriter' w m             => 'R_IndexedGetter' i w u     -> m a -> m (a, (i, u))
-- 'ilistening' :: 'MonadWriter' w m             => 'R_IndexedLens'' i w u      -> m a -> m (a, (i, u))
-- 'ilistening' :: ('MonadWriter' w m, 'Monoid' u) => 'R_IndexedFold' i w u       -> m a -> m (a, (i, u))
-- 'ilistening' :: ('MonadWriter' w m, 'Monoid' u) => 'R_IndexedTraversal'' i w u -> m a -> m (a, (i, u))
-- @
ilistening :: (MonadWriter w m, AsIndexedGetting i (i, u) k) => Optic' k w u -> m a -> m (a, (i, u))
ilistening l m = do
  (a, w) <- listen m
  return (a, iview l w)
{-# INLINE ilistening #-}

-- | This is a generalized form of 'listen' that only extracts the portion of
-- the log that is focused on by a 'R_Getter'. If given a 'R_Fold' or a 'R_Traversal'
-- then a monoidal summary of the parts of the log that are visited will be
-- returned.
--
-- @
-- 'listenings' :: 'MonadWriter' w m             => 'R_Getter' w u     -> (u -> v) -> m a -> m (a, v)
-- 'listenings' :: 'MonadWriter' w m             => 'R_Lens'' w u      -> (u -> v) -> m a -> m (a, v)
-- 'listenings' :: 'MonadWriter' w m             => 'R_Iso'' w u       -> (u -> v) -> m a -> m (a, v)
-- 'listenings' :: ('MonadWriter' w m, 'Monoid' v) => 'R_Fold' w u       -> (u -> v) -> m a -> m (a, v)
-- 'listenings' :: ('MonadWriter' w m, 'Monoid' v) => 'R_Traversal'' w u -> (u -> v) -> m a -> m (a, v)
-- 'listenings' :: ('MonadWriter' w m, 'Monoid' v) => 'R_Prism'' w u     -> (u -> v) -> m a -> m (a, v)
-- @
listenings :: forall u v w m k a. (MonadWriter w m, AsGetting v k) => Optic' k w u -> (u -> v) -> m a -> m (a, v)
listenings l uv m = do
  (a, w) <- listen m
  return (a, views (asLensLike (asGetting l :: Getting v w u)) uv w)
{-# INLINE listenings #-}

-- | This is a generalized form of 'listen' that only extracts the portion of
-- the log that is focused on by a 'R_Getter'. If given a 'R_Fold' or a 'R_Traversal'
-- then a monoidal summary of the parts of the log that are visited will be
-- returned.
--
-- @
-- 'ilistenings' :: 'MonadWriter' w m             => 'R_IndexedGetter' w u     -> (i -> u -> v) -> m a -> m (a, v)
-- 'ilistenings' :: 'MonadWriter' w m             => 'R_IndexedLens'' w u      -> (i -> u -> v) -> m a -> m (a, v)
-- 'ilistenings' :: ('MonadWriter' w m, 'Monoid' v) => 'R_IndexedFold' w u       -> (i -> u -> v) -> m a -> m (a, v)
-- 'ilistenings' :: ('MonadWriter' w m, 'Monoid' v) => 'R_IndexedTraversal'' w u -> (i -> u -> v) -> m a -> m (a, v)
-- @
ilistenings :: (MonadWriter w m, AsIndexedGetting i v k) => Optic' k w u -> (i -> u -> v) -> m a -> m (a, v)
ilistenings l iuv m = do
  (a, w) <- listen m
  return (a, iviews l iuv w)
{-# INLINE ilistenings #-}

------------------------------------------------------------------------------
-- Indexed R_Getters
------------------------------------------------------------------------------

-- | View the index and value of an 'R_IndexedGetter' into the current environment as a pair.
--
-- When applied to an 'R_IndexedFold' the result will most likely be a nonsensical monoidal summary of
-- the indices tupled with a monoidal summary of the values and probably not whatever it is you wanted.
iview :: (MonadReader s m, AsIndexedGetting i (i, a) k) => Optic' k s a -> m (i,a)
iview l = asks (getConst #. runOptic (asIndexedGetting l) (Indexed $ \i -> Const #. (,) i))
{-# INLINE iview #-}

-- | View a function of the index and value of an 'R_IndexedGetter' into the current environment.
--
-- When applied to an 'R_IndexedFold' the result will be a monoidal summary instead of a single answer.
--
-- @
-- 'iviews' ≡ 'Control.R_Lens.R_Fold.ifoldMapOf'
-- @
iviews :: (MonadReader s m, AsIndexedGetting i r k) => Optic' k s a -> (i -> a -> r) -> m r
iviews l f = asks (getConst #. runOptic (asIndexedGetting l) (Const #. Indexed f))
{-# INLINE iviews #-}

-- | Use the index and value of an 'R_IndexedGetter' into the current state as a pair.
--
-- When applied to an 'R_IndexedFold' the result will most likely be a nonsensical monoidal summary of
-- the indices tupled with a monoidal summary of the values and probably not whatever it is you wanted.
iuse :: (MonadState s m, AsIndexedGetting i (i,a) k) => Optic' k s a -> m (i,a)
iuse l = gets (getConst #. runOptic (asIndexedGetting l) (Indexed $ \i -> Const #. (,) i))
{-# INLINE iuse #-}

-- | Use a function of the index and value of an 'R_IndexedGetter' into the current state.
--
-- When applied to an 'R_IndexedFold' the result will be a monoidal summary instead of a single answer.
iuses :: (MonadState s m, AsIndexedGetting i r k) => Optic' k s a -> (i -> a -> r) -> m r
iuses l f = gets (getConst #. runOptic (asIndexedGetting l) (Const #. Indexed f))
{-# INLINE iuses #-}

-- | View the index and value of an 'R_IndexedGetter' or 'R_IndexedLens'.
--
-- This is the same operation as 'iview' with the arguments flipped.
--
-- The fixity and semantics are such that subsequent field accesses can be
-- performed with ('Prelude..').
--
-- @
-- ('^@.') :: s -> 'R_IndexedGetter' i s a -> (i, a)
-- ('^@.') :: s -> 'R_IndexedLens'' i s a  -> (i, a)
-- @
--
-- The result probably doesn't have much meaning when applied to an 'R_IndexedFold'.
(^@.) :: AsIndexedGetting i (i, a) k => s -> Optic' k s a -> (i, a)
s ^@. l = getConst $ runOptic (asIndexedGetting l) (Indexed $ \i -> Const #. (,) i) s
{-# INLINE (^@.) #-}

-- | Coerce a 'R_Getter'-compatible 'R_Optical' to an 'R_Optical''. This
-- is useful when using a 'R_Traversal' that is not simple as a 'R_Getter' or a
-- 'R_Fold'.
--
-- @
-- 'getting' :: 'R_Traversal' s t a b          -> 'R_Fold' s a
-- 'getting' :: 'R_Lens' s t a b               -> 'R_Getter' s a
-- 'getting' :: 'R_IndexedTraversal' i s t a b -> 'R_IndexedFold' i s a
-- 'getting' :: 'R_IndexedLens' i s t a b      -> 'R_IndexedGetter' i s a
-- @
rgetting :: (Profunctor p, Profunctor q, Functor f, Contravariant f)
        => R_Optical p q f s t a b -> R_Optical' p q f s a
rgetting l f = rmap phantom . l $ rmap phantom f

asOptical :: AsOptical p q f k => Optic k s t a b -> Optical p q f s t a b
asOptical = sub

ggetting :: (Profunctor p, Profunctor q, Functor f, Contravariant f, k <: A_Optical p q f)
         => Optic k s t a b -> Optical' p q f s a
ggetting l = Optic (rgetting (runOptic (asOptical l)))

pgetting :: (Profunctor p, Profunctor q, Functor f, Contravariant f)
        => Optical p q f s t a b -> Optical' p q f s a
pgetting l = Optic (rgetting (runOptic l))
