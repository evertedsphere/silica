{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeInType #-}
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Silica.Type
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- This module exports the majority of the types that need to appear in user
-- signatures or in documentation when talking about lenses. The remaining types
-- for consuming lenses are distributed across various modules in the hierarchy.
-------------------------------------------------------------------------------
module Silica.Type where

  -- (
  -- -- * Other
  --   R_Equality, R_Equality', R_As
  -- , R_Iso, R_Iso'
  -- , R_Prism , R_Prism'
  -- , R_Review , R_AReview
  -- -- * R_Lenses, R_Folds and R_Traversals
  -- , R_Lens, R_Lens'
  -- , R_Traversal, R_Traversal'
  -- , R_Traversal1, R_Traversal1'
  -- , R_Setter, R_Setter'
  -- , R_Getter, R_Fold
  -- , R_Fold1
  -- -- * Indexed
  -- , R_IndexedLens, R_IndexedLens'
  -- , R_IndexedTraversal, R_IndexedTraversal'
  -- , R_IndexedTraversal1, R_IndexedTraversal1'
  -- , R_IndexedSetter, R_IndexedSetter'
  -- , R_IndexedGetter, R_IndexedFold
  -- , R_IndexedFold1
  -- -- * Index-Preserving
  -- , R_IndexPreservingLens, R_IndexPreservingLens'
  -- , R_IndexPreservingTraversal, R_IndexPreservingTraversal'
  -- , R_IndexPreservingTraversal1, R_IndexPreservingTraversal1'
  -- , R_IndexPreservingSetter, R_IndexPreservingSetter'
  -- , R_IndexPreservingGetter, R_IndexPreservingFold
  -- , R_IndexPreservingFold1
  -- -- * Common
  -- , Simple
  -- , R_LensLike, R_LensLike'
  -- , R_Over, R_Over'
  -- , R_IndexedLensLike, R_IndexedLensLike'
  -- , R_Optical, R_Optical'
  -- , R_Optic, R_Optic'

  -- -- * Other
  -- , Equality, Equality'
  -- , Iso, Iso'
  -- , Prism , Prism'
  -- , Review 
  -- -- * Lenses, Folds and Traversals
  -- , Lens, Lens'
  -- , Traversal, Traversal'
  -- , Traversal1, Traversal1'
  -- , Setter, Setter'
  -- , Getter, Fold
  -- , Fold1
  -- -- * Indexed
  -- , IndexedLens, IndexedLens'
  -- , IndexedTraversal, IndexedTraversal'
  -- , IndexedTraversal1, IndexedTraversal1'
  -- , IndexedSetter, IndexedSetter'
  -- , IndexedGetter, IndexedFold
  -- , IndexedFold1
  -- -- * Index-Preserving
  -- -- , IndexPreservingLens, IndexPreservingLens'
  -- -- , IndexPreservingTraversal, IndexPreservingTraversal'
  -- -- , IndexPreservingTraversal1, IndexPreservingTraversal1'
  -- -- , IndexPreservingSetter, IndexPreservingSetter'
  -- -- , IndexPreservingGetter, IndexPreservingFold
  -- -- , IndexPreservingFold1
  -- -- * Common
  -- , Optic(..), Optic'

  -- , AsL
  -- ) where

import Control.Applicative
import Silica.Internal.Setter
import Silica.Internal.Indexed
import Data.Bifunctor
import Data.Functor.Identity
import Data.Functor.Contravariant
import Data.Functor.Apply
#if __GLASGOW_HASKELL__ >= 800
import Data.Kind
#endif
import Data.Profunctor
import Data.Tagged
import Prelude
import Data.Semigroup

import GHC.TypeLits

{-# ANN module "HLint: ignore Use camelCase" #-}

-- $setup
-- >>> :set -XNoR_OverloadedStrings
-- >>> import Silica
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g,h)
-- >>> import Prelude
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let h :: Expr -> Expr -> Expr; h = Debug.SimpleReflect.Vars.h
-- >>> let getter :: Expr -> Expr; getter = fun "getter"
-- >>> let setter :: Expr -> Expr -> Expr; setter = fun "setter"
-- >>> import Numeric.Natural
-- >>> let nat :: R_Prism' Integer Natural; nat = prism toInteger $ \i -> if i < 0 then Left i else Right (fromInteger i)

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

type R_Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | @
-- type 'R_Lens'' = 'Simple' 'R_Lens'
-- @
type R_Lens' s a = R_Lens s s a a

-- | Every 'R_IndexedLens' is a valid 'R_Lens' and a valid 'Control.R_Lens.R_Traversal.R_IndexedTraversal'.
type R_IndexedLens i s t a b = forall f p. (Indexable i p, Functor f) => p a (f b) -> s -> f t

-- | @
-- type 'R_IndexedLens'' i = 'Simple' ('R_IndexedLens' i)
-- @
type R_IndexedLens' i s a = R_IndexedLens i s s a a

-- | An 'R_IndexPreservingLens' leaves any index it is composed with alone.
type R_IndexPreservingLens s t a b = forall p f. (Conjoined p, Functor f) => p a (f b) -> p s (f t)

-- | @
-- type 'R_IndexPreservingLens'' = 'Simple' 'R_IndexPreservingLens'
-- @
type R_IndexPreservingLens' s a = R_IndexPreservingLens s s a a

------------------------------------------------------------------------------
-- R_Traversals
------------------------------------------------------------------------------

-- | A 'R_Traversal' can be used directly as a 'Control.R_Lens.R_Setter.R_Setter' or a 'R_Fold' (but not as a 'R_Lens') and provides
-- the ability to both read and update multiple fields, subject to some relatively weak 'R_Traversal' laws.
--
-- These have also been known as multilenses, but they have the signature and spirit of
--
-- @
-- 'Data.Traversable.traverse' :: 'Data.Traversable.Traversable' f => 'R_Traversal' (f a) (f b) a b
-- @
--
-- and the more evocative name suggests their application.
--
-- Most of the time the 'R_Traversal' you will want to use is just 'Data.Traversable.traverse', but you can also pass any
-- 'R_Lens' or 'R_Iso' as a 'R_Traversal', and composition of a 'R_Traversal' (or 'R_Lens' or 'R_Iso') with a 'R_Traversal' (or 'R_Lens' or 'R_Iso')
-- using ('.') forms a valid 'R_Traversal'.
--
-- The laws for a 'R_Traversal' @t@ follow from the laws for 'Data.Traversable.Traversable' as stated in \"The Essence of the Iterator Pattern\".
--
-- @
-- t 'pure' ≡ 'pure'
-- 'fmap' (t f) '.' t g ≡ 'Data.Functor.Compose.getCompose' '.' t ('Data.Functor.Compose.Compose' '.' 'fmap' f '.' g)
-- @
--
-- One consequence of this requirement is that a 'R_Traversal' needs to leave the same number of elements as a
-- candidate for subsequent 'R_Traversal' that it started with. Another testament to the strength of these laws
-- is that the caveat expressed in section 5.5 of the \"Essence of the Iterator Pattern\" about exotic
-- 'Data.Traversable.Traversable' instances that 'Data.Traversable.traverse' the same entry multiple times was actually already ruled out by the
-- second law in that same paper!
type R_Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- | @
-- type 'R_Traversal'' = 'Simple' 'R_Traversal'
-- @
type R_Traversal' s a = R_Traversal s s a a

type R_Traversal1 s t a b = forall f. Apply f => (a -> f b) -> s -> f t
type R_Traversal1' s a = R_Traversal1 s s a a

-- | Every 'R_IndexedTraversal' is a valid 'Control.R_Lens.R_Traversal.R_Traversal' or
-- 'Control.R_Lens.R_Fold.R_IndexedFold'.
--
-- The 'Indexed' constraint is used to allow an 'R_IndexedTraversal' to be used
-- directly as a 'Control.R_Lens.R_Traversal.R_Traversal'.
--
-- The 'Control.R_Lens.R_Traversal.R_Traversal' laws are still required to hold.
--
-- In addition, the index @i@ should satisfy the requirement that it stays
-- unchanged even when modifying the value @a@, otherwise traversals like
-- 'indices' break the 'R_Traversal' laws.
type R_IndexedTraversal i s t a b = forall p f. (Indexable i p, Applicative f) => p a (f b) -> s -> f t

-- | @
-- type 'R_IndexedTraversal'' i = 'Simple' ('R_IndexedTraversal' i)
-- @
type R_IndexedTraversal' i s a = R_IndexedTraversal i s s a a

type R_IndexedTraversal1 i s t a b = forall p f. (Indexable i p, Apply f) => p a (f b) -> s -> f t
type R_IndexedTraversal1' i s a = R_IndexedTraversal1 i s s a a

-- | An 'R_IndexPreservingLens' leaves any index it is composed with alone.
type R_IndexPreservingTraversal s t a b = forall p f. (Conjoined p, Applicative f) => p a (f b) -> p s (f t)

-- | @
-- type 'R_IndexPreservingTraversal'' = 'Simple' 'R_IndexPreservingTraversal'
-- @
type R_IndexPreservingTraversal' s a = R_IndexPreservingTraversal s s a a

type R_IndexPreservingTraversal1 s t a b = forall p f. (Conjoined p, Apply f) => p a (f b) -> p s (f t)
type R_IndexPreservingTraversal1' s a = R_IndexPreservingTraversal1 s s a a

------------------------------------------------------------------------------
-- R_Setters
------------------------------------------------------------------------------

-- | The only 'R_LensLike' law that can apply to a 'R_Setter' @l@ is that
--
-- @
-- 'Control.R_Lens.R_Setter.set' l y ('Control.R_Lens.R_Setter.set' l x a) ≡ 'Control.R_Lens.R_Setter.set' l y a
-- @
--
-- You can't 'Control.R_Lens.R_Getter.view' a 'R_Setter' in general, so the other two laws are irrelevant.
--
-- However, two 'Functor' laws apply to a 'R_Setter':
--
-- @
-- 'Control.R_Lens.R_Setter.over' l 'id' ≡ 'id'
-- 'Control.R_Lens.R_Setter.over' l f '.' 'Control.R_Lens.R_Setter.over' l g ≡ 'Control.R_Lens.R_Setter.over' l (f '.' g)
-- @
--
-- These can be stated more directly:
--
-- @
-- l 'pure' ≡ 'pure'
-- l f '.' 'untainted' '.' l g ≡ l (f '.' 'untainted' '.' g)
-- @
--
-- You can compose a 'R_Setter' with a 'R_Lens' or a 'R_Traversal' using ('.') from the @Prelude@
-- and the result is always only a 'R_Setter' and nothing more.
--
-- >>> over traverse f [a,b,c,d]
-- [f a,f b,f c,f d]
--
-- >>> over _1 f (a,b)
-- (f a,b)
--
-- >>> over (traverse._1) f [(a,b),(c,d)]
-- [(f a,b),(f c,d)]
--
-- >>> over both f (a,b)
-- (f a,f b)
--
-- >>> over (traverse.both) f [(a,b),(c,d)]
-- [(f a,f b),(f c,f d)]
type R_Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t

-- | A 'R_Setter'' is just a 'R_Setter' that doesn't change the types.
--
-- These are particularly common when talking about monomorphic containers. /e.g./
--
-- @
-- 'sets' Data.Text.map :: 'R_Setter'' 'Data.Text.Internal.Text' 'Char'
-- @
--
-- @
-- type 'R_Setter'' = 'Simple' 'R_Setter'
-- @
type R_Setter' s a = R_Setter s s a a

-- | Every 'R_IndexedSetter' is a valid 'R_Setter'.
--
-- The 'R_Setter' laws are still required to hold.
type R_IndexedSetter i s t a b = forall f p.
  (Indexable i p, Settable f) => p a (f b) -> s -> f t

-- | @
-- type 'R_IndexedSetter'' i = 'Simple' ('R_IndexedSetter' i)
-- @
type R_IndexedSetter' i s a = R_IndexedSetter i s s a a

-- | An 'R_IndexPreservingSetter' can be composed with a 'R_IndexedSetter', 'R_IndexedTraversal' or 'R_IndexedLens'
-- and leaves the index intact, yielding an 'R_IndexedSetter'.
type R_IndexPreservingSetter s t a b = forall p f. (Conjoined p, Settable f) => p a (f b) -> p s (f t)

-- | @
-- type 'IndexedPreservingR_Setter'' i = 'Simple' 'IndexedPreservingR_Setter'
-- @
type R_IndexPreservingSetter' s a = R_IndexPreservingSetter s s a a

-----------------------------------------------------------------------------
-- R_Isomorphisms
-----------------------------------------------------------------------------

-- | R_Isomorphism families can be composed with another 'R_Lens' using ('.') and 'id'.
--
-- Since every 'R_Iso' is both a valid 'R_Lens' and a valid 'R_Prism', the laws for those types
-- imply the following laws for an 'R_Iso' 'f':
--
-- @
-- f '.' 'Control.R_Lens.R_Iso.from' f ≡ 'id'
-- 'Control.R_Lens.R_Iso.from' f '.' f ≡ 'id'
-- @
--
-- Note: Composition with an 'R_Iso' is index- and measure- preserving.
type R_Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- | @
-- type 'R_Iso'' = 'Control.R_Lens.Type.Simple' 'R_Iso'
-- @
type R_Iso' s a = R_Iso s s a a

------------------------------------------------------------------------------
-- R_Review Internals
------------------------------------------------------------------------------

-- | This is a limited form of a 'R_Prism' that can only be used for 're' operations.
--
-- Like with a 'R_Getter', there are no laws to state for a 'R_Review'.
--
-- You can generate a 'R_Review' by using 'unto'. You can also use any 'R_Prism' or 'R_Iso'
-- directly as a 'R_Review'.
type R_Review t b = forall p f. (Choice p, Bifunctor p, Settable f) => R_Optic' p f t b

-- | If you see this in a signature for a function, the function is expecting a 'R_Review'
-- (in practice, this usually means a 'R_Prism').
type R_AReview t b = R_Optic' Tagged Identity t b

------------------------------------------------------------------------------
-- R_Prism Internals
------------------------------------------------------------------------------

-- | A 'R_Prism' @l@ is a 'R_Traversal' that can also be turned
-- around with 'Control.R_Lens.R_Review.re' to obtain a 'R_Getter' in the
-- opposite direction.
--
-- There are two laws that a 'R_Prism' should satisfy:
--
-- First, if I 'Control.R_Lens.R_Review.re' or 'Control.R_Lens.R_Review.review' a value with a 'R_Prism' and then 'Control.R_Lens.R_Fold.preview' or use ('Control.R_Lens.R_Fold.^?'), I will get it back:
--
-- @
-- 'Control.R_Lens.R_Fold.preview' l ('Control.R_Lens.R_Review.review' l b) ≡ 'Just' b
-- @
--
-- Second, if you can extract a value @a@ using a 'R_Prism' @l@ from a value @s@, then the value @s@ is completely described by @l@ and @a@:
--
-- If @'Control.R_Lens.R_Fold.preview' l s ≡ 'Just' a@ then @'Control.R_Lens.R_Review.review' l a ≡ s@
--
-- These two laws imply that the 'R_Traversal' laws hold for every 'R_Prism' and that we 'Data.Traversable.traverse' at most 1 element:
--
-- @
-- 'Control.R_Lens.R_Fold.lengthOf' l x '<=' 1
-- @
--
-- It may help to think of this as a 'R_Iso' that can be partial in one direction.
--
-- Every 'R_Prism' is a valid 'R_Traversal'.
--
-- Every 'R_Iso' is a valid 'R_Prism'.
--
-- For example, you might have a @'R_Prism'' 'Integer' 'Numeric.Natural.Natural'@ allows you to always
-- go from a 'Numeric.Natural.Natural' to an 'Integer', and provide you with tools to check if an 'Integer' is
-- a 'Numeric.Natural.Natural' and/or to edit one if it is.
--
--
-- @
-- 'nat' :: 'R_Prism'' 'Integer' 'Numeric.Natural.Natural'
-- 'nat' = 'Control.R_Lens.R_Prism.prism' 'toInteger' '$' \\ i ->
--    if i '<' 0
--    then 'Left' i
--    else 'Right' ('fromInteger' i)
-- @
--
-- Now we can ask if an 'Integer' is a 'Numeric.Natural.Natural'.
--
-- >>> 5^?nat
-- Just 5
--
-- >>> (-5)^?nat
-- Nothing
--
-- We can update the ones that are:
--
-- >>> (-3,4) & both.nat *~ 2
-- (-3,8)
--
-- And we can then convert from a 'Numeric.Natural.Natural' to an 'Integer'.
--
-- >>> 5 ^. re nat -- :: Natural
-- 5
--
-- Similarly we can use a 'R_Prism' to 'Data.Traversable.traverse' the 'Left' half of an 'Either':
--
-- >>> Left "hello" & _Left %~ length
-- Left 5
--
-- or to construct an 'Either':
--
-- >>> 5^.re _Left
-- Left 5
--
-- such that if you query it with the 'R_Prism', you will get your original input back.
--
-- >>> 5^.re _Left ^? _Left
-- Just 5
--
-- Another interesting way to think of a 'R_Prism' is as the categorical dual of a 'R_Lens'
-- -- a co-'R_Lens', so to speak. This is what permits the construction of 'Control.R_Lens.R_Prism.outside'.
--
-- Note: Composition with a 'R_Prism' is index-preserving.
type R_Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

-- | A 'Simple' 'R_Prism'.
type R_Prism' s a = R_Prism s s a a

-------------------------------------------------------------------------------
-- R_Equality
-------------------------------------------------------------------------------

-- | A witness that @(a ~ s, b ~ t)@.
--
-- Note: Composition with an 'R_Equality' is index-preserving.
#if __GLASGOW_HASKELL__ >= 800
type R_Equality (s :: k1) (t :: k2) (a :: k1) (b :: k2) = forall k3 (p :: k1 -> k3 -> *) (f :: k2 -> k3) .
#elif __GLASGOW_HASKELL__ >= 706
type R_Equality (s :: k1) (t :: k2) (a :: k1) (b :: k2) = forall (p :: k1 -> * -> *) (f :: k2 -> *) .
#else
type R_Equality s t a b = forall p (f :: * -> *) .
#endif
    p a (f b) -> p s (f t)

-- | A 'Simple' 'R_Equality'.
type R_Equality' s a = R_Equality s s a a

-- | Composable `asTypeOf`. Useful for constraining excess
-- polymorphism, @foo . (id :: R_As Int) . bar@.
type R_As a = R_Equality' a a

-------------------------------------------------------------------------------
-- R_Getters
-------------------------------------------------------------------------------

-- | A 'R_Getter' describes how to retrieve a single value in a way that can be
-- composed with other 'R_LensLike' constructions.
--
-- Unlike a 'R_Lens' a 'R_Getter' is read-only. Since a 'R_Getter'
-- cannot be used to write back there are no 'R_Lens' laws that can be applied to
-- it. In fact, it is isomorphic to an arbitrary function from @(s -> a)@.
--
-- Moreover, a 'R_Getter' can be used directly as a 'Control.R_Lens.R_Fold.R_Fold',
-- since it just ignores the 'Applicative'.
type R_Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

-- | Every 'R_IndexedGetter' is a valid 'Control.R_Lens.R_Fold.R_IndexedFold' and can be used for 'Control.R_Lens.R_Getter.Getting' like a 'R_Getter'.
type R_IndexedGetter i s a = forall p f. (Indexable i p, Contravariant f, Functor f) => p a (f a) -> s -> f s

-- | An 'R_IndexPreservingGetter' can be used as a 'R_Getter', but when composed with an 'R_IndexedTraversal',
-- 'R_IndexedFold', or 'R_IndexedLens' yields an 'R_IndexedFold', 'R_IndexedFold' or 'R_IndexedGetter' respectively.
type R_IndexPreservingGetter s a = forall p f. (Conjoined p, Contravariant f, Functor f) => p a (f a) -> p s (f s)

--------------------------
-- R_Folds
--------------------------

-- | A 'R_Fold' describes how to retrieve multiple values in a way that can be composed
-- with other 'R_LensLike' constructions.
--
-- A @'R_Fold' s a@ provides a structure with operations very similar to those of the 'Data.R_Foldable.R_Foldable'
-- typeclass, see 'Control.R_Lens.R_Fold.foldMapOf' and the other 'R_Fold' combinators.
--
-- By convention, if there exists a 'foo' method that expects a @'Data.R_Foldable.R_Foldable' (f a)@, then there should be a
-- @fooOf@ method that takes a @'R_Fold' s a@ and a value of type @s@.
--
-- A 'R_Getter' is a legal 'R_Fold' that just ignores the supplied 'Data.Monoid.Monoid'.
--
-- Unlike a 'Control.R_Lens.R_Traversal.R_Traversal' a 'R_Fold' is read-only. Since a 'R_Fold' cannot be used to write back
-- there are no 'R_Lens' laws that apply.
type R_Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

-- | Every 'R_IndexedFold' is a valid 'Control.R_Lens.R_Fold.R_Fold' and can be used for 'Control.R_Lens.R_Getter.Getting'.
type R_IndexedFold i s a = forall p f.  (Indexable i p, Contravariant f, Applicative f) => p a (f a) -> s -> f s

-- | An 'R_IndexPreservingFold' can be used as a 'R_Fold', but when composed with an 'R_IndexedTraversal',
-- 'R_IndexedFold', or 'R_IndexedLens' yields an 'R_IndexedFold' respectively.
type R_IndexPreservingFold s a = forall p f. (Conjoined p, Contravariant f, Applicative f) => p a (f a) -> p s (f s)

-- | A relevant R_Fold (aka 'R_Fold1') has one or more targets.
type R_Fold1 s a = forall f. (Contravariant f, Apply f) => (a -> f a) -> s -> f s
type R_IndexedFold1 i s a = forall p f.  (Indexable i p, Contravariant f, Apply f) => p a (f a) -> s -> f s
type R_IndexPreservingFold1 s a = forall p f. (Conjoined p, Contravariant f, Apply f) => p a (f a) -> p s (f s)

-------------------------------------------------------------------------------
-- Simple R_Overloading
-------------------------------------------------------------------------------

-- | A 'Simple' 'R_Lens', 'Simple' 'R_Traversal', ... can
-- be used instead of a 'R_Lens','R_Traversal', ...
-- whenever the type variables don't change upon setting a value.
--
-- @
-- 'Data.Complex.R_Lens._imagPart' :: 'Simple' 'R_Lens' ('Data.Complex.Complex' a) a
-- 'Control.R_Lens.R_Traversal.traversed' :: 'Simple' ('R_IndexedTraversal' 'Int') [a] a
-- @
--
-- Note: To use this alias in your own code with @'R_LensLike' f@ or
-- 'R_Setter', you may have to turn on @LiberalTypeSynonyms@.
--
-- This is commonly abbreviated as a \"prime\" marker, /e.g./ 'R_Lens'' = 'Simple' 'R_Lens'.
type Simple f s a = f s s a a

-------------------------------------------------------------------------------
-- R_Silica
-------------------------------------------------------------------------------

-- | A valid 'R_Optic' @l@ should satisfy the laws:
--
-- @
-- l 'pure' ≡ 'pure'
-- l ('Procompose' f g) = 'Procompose' (l f) (l g)
-- @
--
-- This gives rise to the laws for 'R_Equality', 'R_Iso', 'R_Prism', 'R_Lens',
-- 'R_Traversal', 'R_Traversal1', 'R_Setter', 'R_Fold', 'R_Fold1', and 'R_Getter' as well
-- along with their index-preserving variants.
--
-- @
-- type 'R_LensLike' f s t a b = 'R_Optic' (->) f s t a b
-- @
type R_Optic p f s t a b = p a (f b) -> p s (f t)

-- | @
-- type 'R_Optic'' p f s a = 'Simple' ('R_Optic' p f) s a
-- @
type R_Optic' p f s a = R_Optic p f s s a a

-- | @
-- type 'R_LensLike' f s t a b = 'R_Optical' (->) (->) f s t a b
-- @
--
-- @
-- type 'R_Over' p f s t a b = 'R_Optical' p (->) f s t a b
-- @
--
-- @
-- type 'R_Optic' p f s t a b = 'R_Optical' p p f s t a b
-- @
type R_Optical p q f s t a b = p a (f b) -> q s (f t)

-- | @
-- type 'R_Optical'' p q f s a = 'Simple' ('R_Optical' p q f) s a
-- @
type R_Optical' p q f s a = R_Optical p q f s s a a


-- | Many combinators that accept a 'R_Lens' can also accept a
-- 'R_Traversal' in limited situations.
--
-- They do so by specializing the type of 'Functor' that they require of the
-- caller.
--
-- If a function accepts a @'R_LensLike' f s t a b@ for some 'Functor' @f@,
-- then they may be passed a 'R_Lens'.
--
-- Further, if @f@ is an 'Applicative', they may also be passed a
-- 'R_Traversal'.
type R_LensLike f s t a b = (a -> f b) -> s -> f t

-- | @
-- type 'R_LensLike'' f = 'Simple' ('R_LensLike' f)
-- @
type R_LensLike' f s a = R_LensLike f s s a a

-- | Convenient alias for constructing indexed lenses and their ilk.
type R_IndexedLensLike i f s t a b = forall p. Indexable i p => p a (f b) -> s -> f t

-- | Convenient alias for constructing simple indexed lenses and their ilk.
type R_IndexedLensLike' i f s a = R_IndexedLensLike i f s s a a

-- | This is a convenient alias for use when you need to consume either indexed or non-indexed lens-likes based on context.
type R_Over p f s t a b = p a (f b) -> s -> f t

-- | This is a convenient alias for use when you need to consume either indexed or non-indexed lens-likes based on context.
--
-- @
-- type 'R_Over'' p f = 'Simple' ('R_Over' p f)
-- @
type R_Over' p f s a = R_Over p f s s a a

--------------------------------------------------------------------------------

type Silica 
  (p :: Type -> Type -> Type) 
  (q :: Type -> Type -> Type) 
  (f :: Type -> Type) 
  (s :: Type) 
  (t :: Type) 
  (a :: Type) 
  (b :: Type) 
  = (a `p` f b) -> (s `q` f t)

type family Cts 
  (o :: Type) 
  (p :: Type -> Type -> Type) 
  (q :: Type -> Type -> Type) 
  (f :: Type -> Type) 
  = (c :: Constraint)

type    Sand    o p q f s t a b     = Cts o p q f => Silica p q f s t a b
type    Glass   o       s t a b     = forall p q f. Sand o p q f s t a b
newtype Optic   o       s t a b     = Optic { runOptic :: Glass o s t a b }

type    Optic'  o       s   a       = Optic o s s a a
type    Glass'  o       s   a       = Glass o s s a a

--------------------------------------------------------------------------------
-- optic tags and TFs
--------------------------------------------------------------------------------

data A_Lens
data A_Traversal
data A_Traversal1
data A_Prism
data A_Iso
data A_Equality
data A_Review
data A_Fold
data A_Fold1
data A_Setter
data A_Getter

data A_Getting (r :: Type)
data A_LensLike (p :: Type -> Type)
data A_Over (p :: Type -> Type -> Type) (f :: Type -> Type)
data A_Optical (p :: Type -> Type -> Type) (q :: Type -> Type -> Type) (f :: Type -> Type)
data A_Proptic (p :: Type -> Type -> Type) (f :: Type -> Type)

data A_Indexed (i :: Type) (o :: Type)
data A_IndexPreserving (o :: Type)

--------------------------------------------------------------------------------
-- optic types
--------------------------------------------------------------------------------

-- vanilla

-- | A 'Lens' is actually a lens family as described in
-- <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- With great power comes great responsibility and a 'Lens' is subject to the
-- three common sense 'Lens' laws:
--
-- 1) You get back what you put in:
--
-- @
-- 'Control.Lens.Getter.view' l ('Control.Lens.Setter.set' l v s)  ≡ v
-- @
--
-- 2) Putting back what you got doesn't change anything:
--
-- @
-- 'Control.Lens.Setter.set' l ('Control.Lens.Getter.view' l s) s  ≡ s
-- @
--
-- 3) Setting twice is the same as setting once:
--
-- @
-- 'Control.Lens.Setter.set' l v' ('Control.Lens.Setter.set' l v s) ≡ 'Control.Lens.Setter.set' l v' s
-- @
--
-- These laws are strong enough that the 4 type parameters of a 'Lens' cannot
-- vary fully independently. For more on how they interact, read the \"Why is
-- it a Lens Family?\" section of
-- <http://comonad.com/reader/2012/mirrored-lenses/>.
--
-- There are some emergent properties of these laws:
--
-- 1) @'Control.Lens.Setter.set' l s@ must be injective for every @s@ This is a consequence of law #1
--
-- 2) @'Control.Lens.Setter.set' l@ must be surjective, because of law #2, which indicates that it is possible to obtain any 'v' from some 's' such that @'Control.Lens.Setter.set' s v = s@
--
-- 3) Given just the first two laws you can prove a weaker form of law #3 where the values @v@ that you are setting match:
--
-- @
-- 'Control.Lens.Setter.set' l v ('Control.Lens.Setter.set' l v s) ≡ 'Control.Lens.Setter.set' l v s
-- @
--
-- Every 'Lens' can be used directly as a 'Control.Lens.Setter.Setter' or 'Traversal'.
--
-- You can also use a 'Lens' for 'Control.Lens.Getter.Getting' as if it were a
-- 'Fold' or 'Getter'.
--
-- Since every 'Lens' is a valid 'Traversal', the
-- 'Traversal' laws are required of any 'Lens' you create:
--
-- @
-- l 'pure' ≡ 'pure'
-- 'fmap' (l f) '.' l g ≡ 'Data.Functor.Compose.getCompose' '.' l ('Data.Functor.Compose.Compose' '.' 'fmap' f '.' g)
-- @
--
-- @
-- type 'Lens' s t a b = forall f. 'Functor' f => 'LensLike' f s t a b
-- @
type Lens                  s t a b = Optic  A_Lens                     s t a b

type Traversal             s t a b = Optic  A_Traversal                s t a b

type Traversal1            s t a b = Optic  A_Traversal1               s t a b

type Setter                s t a b = Optic  A_Setter                   s t a b

type Equality              s t a b = Optic  A_Equality                 s t a b

type Prism                 s t a b = Optic  A_Prism                    s t a b

type Iso                   s t a b = Optic  A_Iso                      s t a b

type Getter                s   a   = Optic' A_Getter                   s   a
type Fold                  s   a   = Optic' A_Fold                     s   a
type Fold1                 s   a   = Optic' A_Fold1                    s   a
type Review                  t   b = Optic' A_Review                     t   b
type Getting             r s   a   = Optic' (A_Getting r)              s   a

type Equality'             s   a   = Equality                          s s a a
type Lens'                 s   a   = Lens                              s s a a
type Traversal'            s   a   = Traversal                         s s a a
type Traversal1'           s   a   = Traversal1                        s s a a
type Setter'               s   a   = Setter                            s s a a
type Prism'                s   a   = Prism                             s s a a
type Iso'                  s   a   = Iso                               s s a a

type LensLike            f s t a b = Optic  (A_LensLike f)             s t a b
type Over            p   f s t a b = Optic  (A_Over p f)               s t a b
type Optical         p q f s t a b = Optic  (A_Optical p q f)          s t a b
type Proptic         p   f s t a b = Optic  (A_Proptic p f)            s t a b

type LensLike'           f s   a   = LensLike                        f s s a a
type Over'           p   f s   a   = Over                        p   f s s a a
type Optical'        p q f s   a   = Optical                     p q f s s a a
type Proptic'        p   f s   a   = Proptic                     p   f s s a a

-- indexed

type IndexedLens          i   s t a b = Optic  (A_Indexed i A_Lens)          s t a b
type IndexedTraversal     i   s t a b = Optic  (A_Indexed i A_Traversal)     s t a b
type IndexedTraversal1    i   s t a b = Optic  (A_Indexed i A_Traversal)     s t a b
type IndexedSetter        i   s t a b = Optic  (A_Indexed i A_Setter)        s t a b
type IndexedGetter        i   s   a   = Optic' (A_Indexed i A_Getter)        s   a

type IndexedGetting       i r s   a   = Optic' (A_Indexed i (A_Getting r))   s   a
type IndexedFold          i   s   a   = Optic' (A_Indexed i A_Fold)          s   a
type IndexedFold1         i   s   a   = Optic' (A_Indexed i A_Fold1)         s   a

type IndexedLens'         i   s   a   = IndexedLens                        i s s a a
type IndexedTraversal'    i   s   a   = IndexedTraversal                   i s s a a
type IndexedTraversal1'   i   s   a   = IndexedTraversal1                  i s s a a
type IndexedSetter'       i   s   a   = IndexedSetter                      i s s a a

type IndexedLensLike           i f s t a b = Optic  (A_Indexed i (A_LensLike f)) s t a b
type IndexedLensLike'          i f s   a   = IndexedLensLike i f s s a a

-- index-preserving

type IndexPreservingLens            s t a b = Optic  (A_IndexPreserving A_Lens)          s t a b
type IndexPreservingTraversal       s t a b = Optic  (A_IndexPreserving A_Traversal)     s t a b
type IndexPreservingTraversal1      s t a b = Optic  (A_IndexPreserving A_Traversal)     s t a b
type IndexPreservingSetter          s t a b = Optic  (A_IndexPreserving A_Setter)        s t a b
type IndexPreservingGetter          s   a   = Optic' (A_IndexPreserving A_Getter)        s   a

-- type IndexPreservingGetting       r s   a   = Optic' (A_IndexPreserving (A_Getting r))   s   a
type IndexPreservingFold            s   a   = Optic' (A_IndexPreserving A_Fold)          s   a
type IndexPreservingFold1           s   a   = Optic' (A_IndexPreserving A_Fold1)         s   a

type IndexPreservingLens'           s   a   = IndexPreservingLens                        s s a a
type IndexPreservingTraversal'      s   a   = IndexPreservingTraversal                   s s a a
type IndexPreservingTraversal1'     s   a   = IndexPreservingTraversal1                  s s a a
type IndexPreservingSetter'         s   a   = IndexPreservingSetter                      s s a a

--------------------------------------------------------------------------------
-- fiddly bits for subtyping and composition
--------------------------------------------------------------------------------

data SubProxy 
  (o :: Type) 
  (l :: Type) 
  (p :: Type -> Type -> Type) 
  (q :: Type -> Type -> Type) 
  (f :: Type -> Type) 
  = SubProxy

sub :: forall o l s t a b . (o <: l) => Optic o s t a b -> Optic l s t a b
sub (Optic o) = Optic (implies' o)
  where
    implies' :: forall p q f. Sand o p q f s t a b -> Sand l p q f s t a b
    implies' = implies (SubProxy @o @l @p @q @f)

subOut :: (o <: l) => (r -> Optic o s t a b) -> r -> Optic l s t a b
subOut f = sub . f

subIn :: (o <: l) => (Optic l s t a b -> r) -> Optic o s t a b -> r
subIn f = f . sub

-- | Read "can act as" or "is".
class    o <: l where 
  implies :: proxy o l p q f -> (Cts o p q f => r) -> (Cts l p q f => r)

-- commutative
class (o <: m, l <: m) => Join o l m | o l -> m

class Chain o where
  (%%) :: Optic o s t u v -> Optic o u v a b -> Optic o s t a b

(%) :: (Join o l m, Chain m) => Optic o s t u v -> Optic l u v a b -> Optic m s t a b
o % o' = sub o %% sub o'

type Fn k = k ~ (->)
type Fn2 p q = (p ~ (->), q ~ (->))

type instance Cts A_Equality               p q f = (p ~ q)
type instance Cts A_Iso                    p q f = (p ~ q, Profunctor p, Functor f)
type instance Cts A_Review                 p q f = (p ~ q, Choice p, Bifunctor p, Settable f)
type instance Cts A_Prism                  p q f = (p ~ q, Choice p, Applicative f)

type instance Cts A_Setter                 p q f = (Fn2 p q, Settable f)

type instance Cts A_Lens                   p q f = (Fn2 p q, Functor f)

type instance Cts A_Getter                 p q f = (Fn2 p q, Contravariant f, Functor f)

type instance Cts A_Traversal              p q f = (Fn2 p q, Applicative f)
type instance Cts A_Traversal1             p q f = (Fn2 p q, Apply f)

type instance Cts A_Fold                   p q f = (Fn2 p q, Contravariant f, Applicative f)
type instance Cts A_Fold1                  p q f = (Fn2 p q, Contravariant f, Apply f)

type instance Cts (A_Getting r)            p q f = (Fn2 p q, f ~ Const r)

type instance Cts (A_LensLike f')          p q f = (Fn2 p q, f ~ f')
type instance Cts (A_Over p' f')           p q f = (Fn q, p ~ p', f ~ f')
type instance Cts (A_Optical p' q' f')     p q f = (p ~ p', q ~ q', f ~ f')
type instance Cts (A_Proptic p'    f')     p q f = (p ~ p', p ~ q, f ~ f')

type instance Cts (A_Indexed i (A_LensLike g))           p q f = (Fn q, f ~ g, Indexable i p)

type instance Cts (A_Indexed i A_Lens)        p q f = (Fn q, Indexable i p, Functor f)
type instance Cts (A_Indexed i A_Traversal)   p q f = (Fn q, Indexable i p, Applicative f)
type instance Cts (A_Indexed i A_Setter)      p q f = (Fn q, Indexable i p, Settable f)
type instance Cts (A_Indexed i A_Fold)        p q f = (Fn q, Indexable i p, Contravariant f, Applicative f)
type instance Cts (A_Indexed i A_Getter)        p q f = (Fn q, Indexable i p, Contravariant f, Applicative f)
type instance Cts (A_Indexed i (A_Getting r)) p q f = (Fn q, p ~ Indexed i, f ~ Const r)

type instance Cts (A_IndexPreserving A_Lens) p q f = (p ~ q, Conjoined p, Functor f)
type instance Cts (A_IndexPreserving A_Traversal) p q f = (p ~ q, Conjoined p, Applicative f)
type instance Cts (A_IndexPreserving A_Traversal1) p q f = (p ~ q, Conjoined p, Apply f)
type instance Cts (A_IndexPreserving A_Setter) p q f = (p ~ q, Conjoined p, Settable f)
type instance Cts (A_IndexPreserving A_Getter) p q f = (p ~ q, Conjoined p, Contravariant f, Functor f)
type instance Cts (A_IndexPreserving A_Fold) p q f = (p ~ q, Conjoined p, Contravariant f, Applicative f)
type instance Cts (A_IndexPreserving A_Fold1) p q f = (p ~ q, Conjoined p, Contravariant f, Apply f)

----------------------------------------------------------------------
-- composing optics
----------------------------------------------------------------------

instance Chain A_Lens where l %% r = Optic (runOptic l . runOptic r)
instance Chain A_Traversal where l %% r = Optic (runOptic l . runOptic r)
instance Chain A_Prism where l %% r = Optic (runOptic l . runOptic r)
instance Chain A_Iso where l %% r = Optic (runOptic l . runOptic r)
instance Chain A_Equality where l %% r = Optic (runOptic l . runOptic r)
instance Chain A_Getter where l %% r = Optic (runOptic l . runOptic r)
instance Chain A_Fold where l %% r = Optic (runOptic l . runOptic r)
instance Chain A_Review where l %% r = Optic (runOptic l . runOptic r)
instance Chain A_Setter where l %% r = Optic (runOptic l . runOptic r)
instance Chain (A_Getting r) where l %% r = Optic (runOptic l . runOptic r)

instance Chain (A_Indexed i A_Fold) where l %% r = Optic (runOptic l . runOptic r)
instance Chain (A_Indexed i A_Lens) where l %% r = Optic (runOptic l . runOptic r)
instance Chain (A_Indexed i A_Setter) where l %% r = Optic (runOptic l . runOptic r)
instance Chain (A_Indexed i A_Traversal) where l %% r = Optic (runOptic l . runOptic r)
-- instance Chain (A_Indexed i A_Fold1) where l %% r = Optic (runOptic l . runOptic r)
-- instance Chain (A_Indexed i (A_Getting r)) where l %% r = Optic (runOptic l . runOptic r)

--------------------------------------------------------------------------------
-- the subtyping lattice
--------------------------------------------------------------------------------

-- instance o <: o where implies _ r = r

instance A_Lens <: A_Lens where implies _ r = r
instance A_Traversal <: A_Traversal where implies _ r = r
instance A_Traversal1 <: A_Traversal1 where implies _ r = r
instance A_Prism <: A_Prism where implies _ r = r
instance A_Iso <: A_Iso where implies _ r = r
instance A_Equality <: A_Equality where implies _ r = r
instance A_Review <: A_Review where implies _ r = r
instance A_Fold <: A_Fold where implies _ r = r
instance A_Fold1 <: A_Fold1 where implies _ r = r
instance A_Setter <: A_Setter where implies _ r = r
instance A_Getter <: A_Getter where implies _ r = r

instance (r ~ s) => A_Getting r <: A_Getting s where implies _ r = r

instance (p ~ p', f ~ f') => A_Over p f <: A_Over p' f' where implies _ r = r

-- | TODO do some form of recursion of the form o <: o' (split Cts into more TFs?)
-- TODO split into more instances
instance (i ~ j, o ~ o') => A_Indexed i o <: A_Indexed j o' where implies _ r = r

instance A_Equality  <: A_Fold      where implies _ r = r
instance A_Getter    <: A_Fold      where implies _ r = r
instance A_Iso       <: A_Fold      where implies _ r = r
instance A_Lens      <: A_Fold      where implies _ r = r
instance A_Prism     <: A_Fold      where implies _ r = r
instance A_Traversal <: A_Fold      where implies _ r = r

instance A_Equality  <: A_Getter    where implies _ r = r
instance A_Iso       <: A_Getter    where implies _ r = r
instance A_Lens      <: A_Getter    where implies _ r = r

instance A_Equality  <: A_Iso       where implies _ r = r

instance A_Equality  <: A_Lens      where implies _ r = r
instance A_Iso       <: A_Lens      where implies _ r = r

instance A_Equality  <: A_Prism     where implies _ r = r
instance A_Iso       <: A_Prism     where implies _ r = r

instance A_Equality  <: A_Review    where implies _ r = r
instance A_Iso       <: A_Review    where implies _ r = r
instance A_Prism     <: A_Review    where implies _ r = r

instance A_Equality  <: A_Setter    where implies _ r = r
instance A_Iso       <: A_Setter    where implies _ r = r
instance A_Lens      <: A_Setter    where implies _ r = r
instance A_Prism     <: A_Setter    where implies _ r = r
instance A_Traversal <: A_Setter    where implies _ r = r

instance A_Equality  <: A_Traversal where implies _ r = r
instance A_Iso       <: A_Traversal where implies _ r = r
instance A_Lens      <: A_Traversal where implies _ r = r
instance A_Prism     <: A_Traversal where implies _ r = r

instance A_Equality                   <: A_Getting r where implies _ r = r
instance A_Getter                     <: A_Getting r where implies _ r = r
instance A_Iso                        <: A_Getting r where implies _ r = r
instance A_Lens                       <: A_Getting r where implies _ r = r
instance Monoid     r => A_Prism      <: A_Getting r where implies _ r = r
instance Monoid     r => A_Fold       <: A_Getting r where implies _ r = r
instance Semigroup  r => A_Fold1      <: A_Getting r where implies _ r = r
instance Monoid     r => A_Traversal  <: A_Getting r where implies _ r = r
instance Semigroup  r => A_Traversal1 <: A_Getting r where implies _ r = r

instance NotSubtypeError A_Setter A_Getter => A_Setter                       <: A_Getting r where implies _ = error "subtype"

instance NotSubtypeError A_Getter A_Setter => A_Getter <: A_Setter where implies _ = error "subtype"

instance NotSubtypeError A_Traversal A_Getter => A_Traversal <: A_Getter where implies _ = error "subtype"

instance NotSubtypeError (A_Getting r) A_Setter => (A_Getting r) <: A_Setter where implies _ = error "subtype"

instance NotSubtypeError (A_Getting r) A_Getter => (A_Getting r) <: A_Getter where implies _ = error "subtype"

instance A_Indexed i A_Lens <: A_Lens where implies _ r = r
instance A_Indexed i A_Fold <: A_Fold where implies _ r = r

instance Monoid r => A_Indexed i A_Fold <: A_Getting r where implies _ r = r

instance A_Equality <: A_LensLike f where implies _ r = r
instance A_Equality <: A_Over (->) f where implies _ r = r

instance Functor f => A_Lens <: A_LensLike f where implies _ r = r
instance Applicative f => A_Traversal <: A_LensLike f where implies _ r = r

instance (p ~ (->), f ~ g, LensLikeCt f g) => A_Over p f <: A_LensLike g where implies _ r = r

instance (p ~ (->), f ~ g, LensLikeCt f g) => A_LensLike f <: A_Over p g where implies _ r = r

instance (f ~ g, LensLikeCt f g) => A_LensLike f <: A_LensLike g where implies _ r = r

instance A_IndexPreserving A_Setter <: A_Setter where implies _ r = r

instance A_IndexPreserving A_Setter <: A_IndexPreserving A_Setter where implies _ r = r

instance cr ~ Const r => A_Getting r <: A_LensLike cr where implies _ r = r

type family LensLikeCt f g :: Constraint where
  LensLikeCt f f = ()
  LensLikeCt f g = TypeError (Text "!!")

--------------------------------------------------------------------------------
-- Join rules
--------------------------------------------------------------------------------

-- instance Join o           o           o

instance Join A_Lens A_Lens A_Lens
instance Join A_Traversal A_Traversal A_Traversal
instance Join A_Traversal1 A_Traversal1 A_Traversal1
instance Join A_Prism A_Prism A_Prism
instance Join A_Iso A_Iso A_Iso
instance Join A_Equality A_Equality A_Equality
instance Join A_Review A_Review A_Review
instance Join A_Fold A_Fold A_Fold
instance Join A_Fold1 A_Fold1 A_Fold1
instance Join A_Setter A_Setter A_Setter
instance Join A_Getter A_Getter A_Getter

instance (r ~ s, s ~ t) => Join (A_Getting r) (A_Getting s) (A_Getting t)

instance (i ~ i', i ~ i'') => Join (A_Indexed i k) (A_Indexed i' k) (A_Indexed i'' k)

instance Join A_Equality  A_Fold      A_Fold
instance Join A_Equality  A_Getter    A_Getter
instance Join A_Equality  A_Iso       A_Iso
instance Join A_Equality  A_Lens      A_Lens
instance Join A_Equality  A_Prism     A_Prism
instance Join A_Equality  A_Review    A_Review
instance Join A_Equality  A_Setter    A_Setter
instance Join A_Equality  A_Traversal A_Traversal
instance Join A_Fold      A_Equality  A_Fold
instance Join A_Fold      A_Getter    A_Fold
instance Join A_Fold      A_Iso       A_Fold
instance Join A_Fold      A_Lens      A_Fold
instance Join A_Fold      A_Prism     A_Fold
instance Join A_Fold      A_Traversal A_Fold
instance Join A_Getter    A_Equality  A_Getter
instance Join A_Getter    A_Fold      A_Fold
instance Join A_Getter    A_Iso       A_Getter
instance Join A_Getter    A_Lens      A_Getter
instance Join A_Getter    A_Prism     A_Fold
instance Join A_Getter    A_Traversal A_Fold
instance Join A_Iso       A_Equality  A_Iso
instance Join A_Iso       A_Fold      A_Fold
instance Join A_Iso       A_Getter    A_Getter
instance Join A_Iso       A_Lens      A_Lens
instance Join A_Iso       A_Prism     A_Prism
instance Join A_Iso       A_Review    A_Review
instance Join A_Iso       A_Setter    A_Setter
instance Join A_Iso       A_Traversal A_Traversal
instance Join A_Lens      A_Equality  A_Lens
instance Join A_Lens      A_Fold      A_Fold
instance Join A_Lens      A_Getter    A_Getter
instance Join A_Lens      A_Iso       A_Lens
instance Join A_Lens      A_Prism     A_Traversal
instance Join A_Lens      A_Setter    A_Setter
instance Join A_Lens      A_Traversal A_Traversal
instance Join A_Prism     A_Equality  A_Prism
instance Join A_Prism     A_Fold      A_Fold
instance Join A_Prism     A_Getter    A_Fold
instance Join A_Prism     A_Iso       A_Prism
instance Join A_Prism     A_Lens      A_Traversal
instance Join A_Prism     A_Review    A_Review
instance Join A_Prism     A_Setter    A_Setter
instance Join A_Prism     A_Traversal A_Traversal
instance Join A_Review    A_Equality  A_Review
instance Join A_Review    A_Iso       A_Review
instance Join A_Review    A_Prism     A_Review
instance Join A_Setter    A_Equality  A_Setter
instance Join A_Setter    A_Iso       A_Setter
instance Join A_Setter    A_Lens      A_Setter
instance Join A_Setter    A_Prism     A_Setter
instance Join A_Setter    A_Traversal A_Setter
instance Join A_Traversal A_Equality  A_Traversal
instance Join A_Traversal A_Fold      A_Fold
instance Join A_Traversal A_Getter    A_Fold
instance Join A_Traversal A_Iso       A_Traversal
instance Join A_Traversal A_Lens      A_Traversal
instance Join A_Traversal A_Prism     A_Traversal
instance Join A_Traversal A_Setter    A_Setter

instance Join (A_Getting r)    A_Lens        (A_Getting r)
instance Join A_Lens          (A_Getting r)  (A_Getting r)

instance Monoid r => Join (A_Getting r)    A_Traversal   (A_Getting r)
instance Monoid r => Join (A_Getting r)    A_Prism       (A_Getting r)
instance Monoid r => Join (A_Getting r)    A_Iso         (A_Getting r)
instance Monoid r => Join (A_Getting r)    A_Equality    (A_Getting r)
instance Monoid r => Join A_Traversal      (A_Getting r) (A_Getting r)
instance Monoid r => Join A_Prism          (A_Getting r) (A_Getting r)
instance Monoid r => Join A_Iso            (A_Getting r) (A_Getting r)
instance Monoid r => Join A_Equality       (A_Getting r) (A_Getting r)

instance Join (A_Indexed i A_Fold) A_Traversal A_Fold
instance Join (A_Indexed i A_Fold) A_Lens A_Fold

--------------------------------------------------------------------------------
-- custom type errors
--------------------------------------------------------------------------------

type family CannotCompose l r where
  CannotCompose l r = TypeError (Text "Cannot compose optics " :<>: ShowType l :<>: Text " and " :<>: ShowType r)

type family ShowOptic b where
  ShowOptic (A_Getting r) = Text "Getting " :<>: ShowType r
  ShowOptic A_Setter = Text "Setter"
  ShowOptic A_Getter = Text "Getter"
  ShowOptic A_Iso = Text "Iso"
  ShowOptic A_Lens = Text "Lens"
  ShowOptic A_Prism = Text "Prism"
  ShowOptic A_Traversal = Text "Traversal"
  ShowOptic A_Review = Text "Review"
  ShowOptic A_Fold = Text "Fold"
  ShowOptic x = Text "Unknown optic: " :<>: ShowType x

type family NotSubtypeError l r where
  NotSubtypeError l r 
       = TypeError 
       ( Text "A function that you used "
    :<>: Text "requires a "
    :<>: ShowOptic r
    :<>: Text " argument. "
    :$$: Text "The optic supplied was a "
    :<>: ShowOptic l
    :<>: Text ","
    :$$: Text "which cannot be upgraded to the former."
    :$$: Text ""
    :$$: NotSubtypeExplanation l r
    :$$: Text ""
       )

type family NotSubtypeExplanation l r where
  NotSubtypeExplanation A_Getter A_Setter 
       = Text "Intuitively, this is because because Getters are read-only: "
    :$$: Text "you can't update or set values using them."
    :$$: Text ""
    :$$: Text "See https://optics-101.com/faq#getter-setter for help."
  NotSubtypeExplanation l r = Text "FIXME: no subtyping relation between " :$$: ShowType l :$$: ShowType r

type AsLens                o = o <: A_Lens
type AsTraversal           o = o <: A_Traversal
type AsTraversal1          o = o <: A_Traversal1
type AsPrism               o = o <: A_Prism
type AsIso                 o = o <: A_Iso
type AsEquality            o = o <: A_Equality
type AsReview              o = o <: A_Review
type AsFold                o = o <: A_Fold
type AsFold1               o = o <: A_Fold1
type AsSetter              o = o <: A_Setter
type AsIndexedSetter i     o = o <: A_Indexed i A_Setter
type AsIndexedGetter i     o = o <: A_Indexed i A_Getter
type AsGetter              o = o <: A_Getter
type AsLensLike f          o = o <: A_LensLike f
type AsIndexedLensLike i f o = o <: A_Indexed i (A_LensLike f)
type AsGetting r           o = o <: A_Getting r
type AsIndexedGetting i r           o = o <: A_Indexed i (A_Getting r)

type Squashing r o = o <: A_Getting (Endo r)

type AsOver p f            o = o <: A_Over p f
type AsOptical p q f       o = o <: A_Optical p q f
type AsProptic p f         o = o <: A_Proptic p f

asOver :: AsOver p f k => Optic k s t a b -> Over p f s t a b
asOver = sub

runOver :: AsOver p f k => Optic k s t a b -> R_Over p f s t a b
runOver = runOptic . asOver

asLensLike :: AsLensLike f k => Optic k s t a b -> LensLike f s t a b
asLensLike = sub

runLensLike :: AsLensLike f k => Optic k s t a b -> R_LensLike f s t a b
runLensLike = runOptic . asLensLike

asLensLikePair :: AsLensLike ((,) b) k => Optic k s t a b -> LensLike ((,) b) s t a b
asLensLikePair = sub

-- | Explicitly cast an optic to a lens.
asLens :: AsLens o => Optic o s t a b -> Lens s t a b
asLens = sub
{-# INLINE asLens #-}

