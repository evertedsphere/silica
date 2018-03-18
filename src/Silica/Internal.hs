{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.Indexed_
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal implementation details for 'Indexed_' lens-likes
----------------------------------------------------------------------------
module Silica.Internal
  (
  -- * An Indexed_ Profunctor
    Indexed_(..)
  -- * Classes
  , Conjoined(..)
  , Ixable(..)
  -- * Indexing_
  , Indexing_(..)
  , indexing
  -- * 64-bit Indexing_
  , Indexing64_(..)
  , indexing64
  -- * Converting to Folds
  , withIndex
  , asIndex
  ) where

import Control.Applicative
import Control.Arrow as Arrow
import Control.Category
import Control.Comonad
import Control.Lens.Internal.Instances ()
import Control.Monad
import Control.Monad.Fix
import Data.Distributive
import Data.Functor.Bind
import Data.Functor.Contravariant
import Data.Int
import Data.Profunctor.Closed
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Traversable
import Prelude hiding ((.),id)
import Data.Profunctor.Unsafe
import Control.Lens.Internal.Coerce

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Numeric.Lens
--
------------------------------------------------------------------------------
-- Conjoined
------------------------------------------------------------------------------

-- | This is a 'Profunctor' that is both 'Corepresentable' by @f@ and 'Representable' by @g@ such
-- that @f@ is left adjoint to @g@. From this you can derive a lot of structure due
-- to the preservation of limits and colimits.
class
  ( Choice p, Corepresentable p, Comonad (Corep p), Traversable (Corep p)
  , Strong p, Representable p, Monad (Rep p), MonadFix (Rep p), Distributive (Rep p)
  , Costrong p, ArrowLoop p, ArrowApply p, ArrowChoice p, Closed p
  ) => Conjoined p where

  -- | 'Conjoined' is strong enough to let us distribute every 'Conjoined'
  -- 'Profunctor' over every Haskell 'Functor'. This is effectively a
  -- generalization of 'fmap'.
  distrib :: Functor f => p a b -> p (f a) (f b)
  distrib = tabulate . collect . sieve
  {-# INLINE distrib #-}

  -- | This permits us to make a decision at an outermost point about whether or not we use an index.
  --
  -- Ideally any use of this function should be done in such a way so that you compute the same answer,
  -- but this cannot be enforced at the type level.
  conjoined :: ((p ~ (->)) => q (a -> b) r) -> q (p a b) r -> q (p a b) r
  conjoined _ r = r
  {-# INLINE conjoined #-}

instance Conjoined (->) where
  distrib = fmap
  {-# INLINE distrib #-}
  conjoined l _ = l
  {-# INLINE conjoined #-}

----------------------------------------------------------------------------
-- Ixable
----------------------------------------------------------------------------

-- | This class permits overloading of function application for things that
-- also admit a notion of a key or index.
class Conjoined p => Ixable i p where
  -- | Build a function from an 'indexed' function.
  indexed :: p a b -> i -> a -> b

instance Ixable i (->) where
  indexed = const
  {-# INLINE indexed #-}

-----------------------------------------------------------------------------
-- Indexed_ Internals
-----------------------------------------------------------------------------

-- | A function with access to a index. This constructor may be useful when you need to store
-- an 'Ixable' in a container to avoid @ImpredicativeTypes@.
--
-- @index :: Indexed_ i a b -> i -> a -> b@
newtype Indexed_ i a b = Indexed_ { runIndexed_ :: i -> a -> b }

instance Functor (Indexed_ i a) where
  fmap g (Indexed_ f) = Indexed_ $ \i a -> g (f i a)
  {-# INLINE fmap #-}

instance Apply (Indexed_ i a) where
  Indexed_ f <.> Indexed_ g = Indexed_ $ \i a -> f i a (g i a)
  {-# INLINE (<.>) #-}

instance Applicative (Indexed_ i a) where
  pure b = Indexed_ $ \_ _ -> b
  {-# INLINE pure #-}
  Indexed_ f <*> Indexed_ g = Indexed_ $ \i a -> f i a (g i a)
  {-# INLINE (<*>) #-}

instance Bind (Indexed_ i a) where
  Indexed_ f >>- k = Indexed_ $ \i a -> runIndexed_ (k (f i a)) i a
  {-# INLINE (>>-) #-}

instance Monad (Indexed_ i a) where
  return = pure
  {-# INLINE return #-}
  Indexed_ f >>= k = Indexed_ $ \i a -> runIndexed_ (k (f i a)) i a
  {-# INLINE (>>=) #-}

instance MonadFix (Indexed_ i a) where
  mfix f = Indexed_ $ \ i a -> let o = runIndexed_ (f o) i a in o
  {-# INLINE mfix #-}

instance Profunctor (Indexed_ i) where
  dimap ab cd ibc = Indexed_ $ \i -> cd . runIndexed_ ibc i . ab
  {-# INLINE dimap #-}
  lmap ab ibc = Indexed_ $ \i -> runIndexed_ ibc i . ab
  {-# INLINE lmap #-}
  rmap bc iab = Indexed_ $ \i -> bc . runIndexed_ iab i
  {-# INLINE rmap #-}
  ( .# ) ibc _ = coerce ibc
  {-# INLINE ( .# ) #-}
  ( #. ) _ = coerce'
  {-# INLINE ( #. ) #-}

instance Closed (Indexed_ i) where
  closed (Indexed_ iab) = Indexed_ $ \i xa x -> iab i (xa x)

instance Costrong (Indexed_ i) where
  unfirst (Indexed_ iadbd) = Indexed_ $ \i a -> let
      (b, d) = iadbd i (a, d)
    in b

instance Sieve (Indexed_ i) ((->) i) where
  sieve = flip . runIndexed_
  {-# INLINE sieve #-}

instance Representable (Indexed_ i) where
  type Rep (Indexed_ i) = (->) i
  tabulate = Indexed_ . flip
  {-# INLINE tabulate #-}

instance Cosieve (Indexed_ i) ((,) i) where
  cosieve = uncurry . runIndexed_
  {-# INLINE cosieve #-}

instance Corepresentable (Indexed_ i) where
  type Corep (Indexed_ i) = (,) i
  cotabulate = Indexed_ . curry
  {-# INLINE cotabulate #-}

instance Choice (Indexed_ i) where
  right' = right
  {-# INLINE right' #-}

instance Strong (Indexed_ i) where
  second' = second
  {-# INLINE second' #-}

instance Category (Indexed_ i) where
  id = Indexed_ (const id)
  {-# INLINE id #-}
  Indexed_ f . Indexed_ g = Indexed_ $ \i -> f i . g i
  {-# INLINE (.) #-}

instance Arrow (Indexed_ i) where
  arr f = Indexed_ (\_ -> f)
  {-# INLINE arr #-}
  first f = Indexed_ (Arrow.first . runIndexed_ f)
  {-# INLINE first #-}
  second f = Indexed_ (Arrow.second . runIndexed_ f)
  {-# INLINE second #-}
  Indexed_ f *** Indexed_ g = Indexed_ $ \i -> f i *** g i
  {-# INLINE (***) #-}
  Indexed_ f &&& Indexed_ g = Indexed_ $ \i -> f i &&& g i
  {-# INLINE (&&&) #-}

instance ArrowChoice (Indexed_ i) where
  left f = Indexed_ (left . runIndexed_ f)
  {-# INLINE left #-}
  right f = Indexed_ (right . runIndexed_ f)
  {-# INLINE right #-}
  Indexed_ f +++ Indexed_ g = Indexed_ $ \i -> f i +++ g i
  {-# INLINE (+++)  #-}
  Indexed_ f ||| Indexed_ g = Indexed_ $ \i -> f i ||| g i
  {-# INLINE (|||) #-}

instance ArrowApply (Indexed_ i) where
  app = Indexed_ $ \ i (f, b) -> runIndexed_ f i b
  {-# INLINE app #-}

instance ArrowLoop (Indexed_ i) where
  loop (Indexed_ f) = Indexed_ $ \i b -> let (c,d) = f i (b, d) in c
  {-# INLINE loop #-}

instance Conjoined (Indexed_ i) where
  distrib (Indexed_ iab) = Indexed_ $ \i fa -> iab i <$> fa
  {-# INLINE distrib #-}

instance i ~ j => Ixable i (Indexed_ j) where
  indexed = runIndexed_
  {-# INLINE indexed #-}

------------------------------------------------------------------------------
-- Indexing_
------------------------------------------------------------------------------

-- | 'Applicative' composition of @'Control.Monad.Trans.State.Lazy.State' 'Int'@ with a 'Functor', used
-- by 'Control.Lens.Indexed_.indexed'.
newtype Indexing_ f a = Indexing_ { runIndexing_ :: Int -> (Int, f a) }

instance Functor f => Functor (Indexing_ f) where
  fmap f (Indexing_ m) = Indexing_ $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Apply f => Apply (Indexing_ f) where
  Indexing_ mf <.> Indexing_ ma = Indexing_ $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <.> fa)
  {-# INLINE (<.>) #-}

instance Applicative f => Applicative (Indexing_ f) where
  pure x = Indexing_ $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing_ mf <*> Indexing_ ma = Indexing_ $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}

instance Contravariant f => Contravariant (Indexing_ f) where
  contramap f (Indexing_ m) = Indexing_ $ \i -> case m i of
    (j, ff) -> (j, contramap f ff)
  {-# INLINE contramap #-}

-- | Transform a 'Control.Lens.Traversal.Traversal' into an 'Control.Lens.Traversal.Indexed_Traversal' or
-- a 'Control.Lens.Fold.Fold' into an 'Control.Lens.Fold.Indexed_Fold', etc.
--
-- @
-- 'indexing' :: 'Control.Lens.Type.Traversal' s t a b -> 'Control.Lens.Type.Indexed_Traversal' 'Int' s t a b
-- 'indexing' :: 'Control.Lens.Type.Prism' s t a b     -> 'Control.Lens.Type.Indexed_Traversal' 'Int' s t a b
-- 'indexing' :: 'Control.Lens.Type.Lens' s t a b      -> 'Control.Lens.Type.Indexed_Lens' 'Int'  s t a b
-- 'indexing' :: 'Control.Lens.Type.Iso' s t a b       -> 'Control.Lens.Type.Indexed_Lens' 'Int' s t a b
-- 'indexing' :: 'Control.Lens.Type.Fold' s a          -> 'Control.Lens.Type.Indexed_Fold' 'Int' s a
-- 'indexing' :: 'Control.Lens.Type.Getter' s a        -> 'Control.Lens.Type.Indexed_Getter' 'Int' s a
-- @
--
-- @'indexing' :: 'Ixable' 'Int' p => 'Control.Lens.Type.LensLike' ('Indexing_' f) s t a b -> 'Control.Lens.Type.Over' p f s t a b@
indexing :: Ixable Int p => ((a -> Indexing_ f b) -> s -> Indexing_ f t) -> p a (f b) -> s -> f t
indexing l iafb s = snd $ runIndexing_ (l (\a -> Indexing_ (\i -> i `seq` (i + 1, indexed iafb i a))) s) 0
{-# INLINE indexing #-}

------------------------------------------------------------------------------
-- Indexing64_
------------------------------------------------------------------------------

-- | 'Applicative' composition of @'Control.Monad.Trans.State.Lazy.State' 'Int64'@ with a 'Functor', used
-- by 'Control.Lens.Indexed_.indexed64'.
newtype Indexing64_ f a = Indexing64_ { runIndexing64_ :: Int64 -> (Int64, f a) }

instance Functor f => Functor (Indexing64_ f) where
  fmap f (Indexing64_ m) = Indexing64_ $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Apply f => Apply (Indexing64_ f) where
  Indexing64_ mf <.> Indexing64_ ma = Indexing64_ $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <.> fa)
  {-# INLINE (<.>) #-}

instance Applicative f => Applicative (Indexing64_ f) where
  pure x = Indexing64_ $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing64_ mf <*> Indexing64_ ma = Indexing64_ $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}

instance Contravariant f => Contravariant (Indexing64_ f) where
  contramap f (Indexing64_ m) = Indexing64_ $ \i -> case m i of
    (j, ff) -> (j, contramap f ff)
  {-# INLINE contramap #-}

-- | Transform a 'Control.Lens.Traversal.Traversal' into an 'Control.Lens.Traversal.Indexed_Traversal' or
-- a 'Control.Lens.Fold.Fold' into an 'Control.Lens.Fold.Indexed_Fold', etc.
--
-- This combinator is like 'indexing' except that it handles large traversals and folds gracefully.
--
-- @
-- 'indexing64' :: 'Control.Lens.Type.Traversal' s t a b -> 'Control.Lens.Type.Indexed_Traversal' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Type.Prism' s t a b     -> 'Control.Lens.Type.Indexed_Traversal' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Type.Lens' s t a b      -> 'Control.Lens.Type.Indexed_Lens' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Type.Iso' s t a b       -> 'Control.Lens.Type.Indexed_Lens' 'Int64' s t a b
-- 'indexing64' :: 'Control.Lens.Type.Fold' s a          -> 'Control.Lens.Type.Indexed_Fold' 'Int64' s a
-- 'indexing64' :: 'Control.Lens.Type.Getter' s a        -> 'Control.Lens.Type.Indexed_Getter' 'Int64' s a
-- @
--
-- @'indexing64' :: 'Ixable' 'Int64' p => 'Control.Lens.Type.LensLike' ('Indexing64_' f) s t a b -> 'Control.Lens.Type.Over' p f s t a b@
indexing64 :: Ixable Int64 p => ((a -> Indexing64_ f b) -> s -> Indexing64_ f t) -> p a (f b) -> s -> f t
indexing64 l iafb s = snd $ runIndexing64_ (l (\a -> Indexing64_ (\i -> i `seq` (i + 1, indexed iafb i a))) s) 0
{-# INLINE indexing64 #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Fold a container with indices returning both the indices and the values.
--
-- The result is only valid to compose in a 'Traversal', if you don't edit the
-- index as edits to the index have no effect.
--
-- >>> [10, 20, 30] ^.. ifolded . withIndex
-- [(0,10),(1,20),(2,30)]
--
-- >>> [10, 20, 30] ^.. ifolded . withIndex . alongside negated (re _Show)
-- [(0,"10"),(-1,"20"),(-2,"30")]
--
withIndex :: (Ixable i p, Functor f) => p (i, s) (f (j, t)) -> Indexed_ i s (f t)
withIndex f = Indexed_ $ \i a -> snd <$> indexed f i (i, a)
{-# INLINE withIndex #-}

-- | When composed with an 'Indexed_Fold' or 'Indexed_Traversal' this yields an
-- ('Indexed_') 'Fold' of the indices.
asIndex :: (Ixable i p, Contravariant f, Functor f) => p i (f i) -> Indexed_ i s (f s)
asIndex f = Indexed_ $ \i _ -> phantom (indexed f i i)
{-# INLINE asIndex #-}
