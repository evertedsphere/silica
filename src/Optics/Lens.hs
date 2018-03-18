{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif

#if __GLASGOW_HASKELL__ < 708
{-# LANGUAGE Trustworthy #-}
#endif
-------------------------------------------------------------------------------
-- |
-- Module      :  Optics.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A @'R_Lens' s t a b@ is a purely functional reference.
--
-- While a 'Control.R_Lens.R_Traversal.R_Traversal' could be used for
-- 'Control.R_Lens.R_Getter.Getting' like a valid 'Control.R_Lens.R_Fold.R_Fold', it
-- wasn't a valid 'Control.R_Lens.R_Getter.R_Getter' as a
-- 'Control.R_Lens.R_Getter.R_Getter' can't require an 'Applicative' constraint.
--
-- 'Functor', however, is a constraint on both.
--
-- @
-- type 'R_Lens' s t a b = forall f. 'Functor' f => (a -> f b) -> s -> f t
-- @
--
-- Every 'R_Lens' is a valid 'Control.R_Lens.R_Setter.R_Setter'.
--
-- Every 'R_Lens' can be used for 'Control.R_Lens.R_Getter.Getting' like a
-- 'Control.R_Lens.R_Fold.R_Fold' that doesn't use the 'Applicative' or
-- 'Contravariant'.
--
-- Every 'R_Lens' is a valid 'Control.R_Lens.R_Traversal.R_Traversal' that only uses
-- the 'Functor' part of the 'Applicative' it is supplied.
--
-- Every 'R_Lens' can be used for 'Control.R_Lens.R_Getter.Getting' like a valid
-- 'Control.R_Lens.R_Getter.R_Getter'.
--
-- Since every 'R_Lens' can be used for 'Control.R_Lens.R_Getter.Getting' like a
-- valid 'Control.R_Lens.R_Getter.R_Getter' it follows that it must view exactly one element in the
-- structure.
--
-- The 'R_Lens' laws follow from this property and the desire for it to act like
-- a 'Data.Traversable.Traversable' when used as a
-- 'Control.R_Lens.R_Traversal.R_Traversal'.
--
-- In the examples below, 'getter' and 'setter' are supplied as example getters
-- and setters, and are not actual functions supplied by this package.
-------------------------------------------------------------------------------
module Optics.Lens
  (
  -- * R_Lenses
    R_Lens, R_Lens'
  , R_IndexedLens, R_IndexedLens'
  -- ** Concrete R_Lenses
  , R_ALens, R_ALens'
  , R_AnIndexedLens, R_AnIndexedLens'

  -- * Combinators
  , lens, ilens, iplens
  , (%%~), (%%=)
  , (%%@~), (%%@=)
  , (<%@~), (<%@=)
  , (<<%@~), (<<%@=)
  -- ** General Purpose Combinators
  , (&), (<&>), (??)
  , (&~)
  -- * Lateral Composition
  , choosing
  , chosen
  , alongside
  , inside

  -- * Setting Functionally with Passthrough
  , (<%~), (<+~), (<-~), (<*~), (<//~)
  , (<^~), (<^^~), (<**~)
  , (<||~), (<&&~), (<<>~)
  , (<<%~), (<<.~), (<<?~), (<<+~), (<<-~), (<<*~)
  , (<<//~), (<<^~), (<<^^~), (<<**~)
  , (<<||~), (<<&&~), (<<<>~)

  -- * Setting State with Passthrough
  , (<%=), (<+=), (<-=), (<*=), (<//=)
  , (<^=), (<^^=), (<**=)
  , (<||=), (<&&=), (<<>=)
  , (<<%=), (<<.=), (<<?=), (<<+=), (<<-=), (<<*=)
  , (<<//=), (<<^=), (<<^^=), (<<**=)
  , (<<||=), (<<&&=), (<<<>=)
  , (<<~)

  -- * Cloning R_Lenses
  , cloneR_Lens
  , cloneR_IndexPreservingLens
  , cloneR_IndexedLens

  -- * Arrow operators
  , overA

  -- * R_ALens Combinators
  , storing
  , (^#)
  , ( #~ ), ( #%~ ), ( #%%~ ), (<#~), (<#%~)
  , ( #= ), ( #%= ), ( #%%= ), (<#=), (<#%=)

  -- * Common R_Lenses
  , devoid
  , united

  -- * Context
  , Context(..)
  , Context'
  , locus

  -- * R_Lens fusion
  , fusing
  ) where

import Control.Applicative
import Control.Arrow
import Control.Comonad
import Optics.Internal.Context
import Optics.Internal.Getter
import Optics.Internal.Indexed
import Optics.Type
import Control.Monad.State as State
import Data.Functor.Yoneda
import Data.Monoid
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Profunctor.Unsafe
import Data.Void
import Prelude
#if MIN_VERSION_base(4,8,0)
import Data.Function ((&))
#endif
#if MIN_VERSION_base(4,11,0)
import Data.Functor ((<&>))
#endif

#ifdef HLINT
{-# ANN module "HLint: ignore Use ***" #-}
#endif

-- $setup
-- >>> :set -XNoR_OverloadedStrings
-- >>> import Optics
-- >>> import Control.Monad.State
-- >>> import Data.Char (chr)
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g,h)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let h :: Expr -> Expr -> Expr; h = Debug.SimpleReflect.Vars.h
-- >>> let getter :: Expr -> Expr; getter = fun "getter"
-- >>> let setter :: Expr -> Expr -> Expr; setter = fun "setter"

infixl 8 ^#

infixr 4 %%@~, <%@~, <<%@~, %%~, <+~, <*~, <-~, <//~, <^~, <^^~, <**~, <&&~
infixr 4 <||~, <<>~, <%~, <<%~, <<.~, <<?~, <#~, #~, #%~, <#%~, #%%~
infixr 4 <<+~, <<-~, <<*~, <<//~, <<^~, <<^^~, <<**~, <<||~, <<&&~, <<<>~

infix  4 %%@=, <%@=, <<%@=, %%=, <+=, <*=, <-=, <//=, <^=, <^^=, <**=, <&&=
infix  4 <||=, <<>=, <%=, <<%=, <<.=, <<?=, <#=, #=, #%=, <#%=, #%%=
infix  4 <<+=, <<-=, <<*=, <<//=, <<^=, <<^^=, <<**=, <<||=, <<&&=, <<<>=

infixr 2 <<~
infixl 1 ??, &~

-------------------------------------------------------------------------------
-- R_Lenses
-------------------------------------------------------------------------------

-- | When you see this as an argument to a function, it expects a 'R_Lens'.
--
-- This type can also be used when you need to store a 'R_Lens' in a container,
-- since it is rank-1. You can turn them back into a 'R_Lens' with 'cloneR_Lens',
-- or use it directly with combinators like 'storing' and ('^#').
type R_ALens s t a b = R_LensLike (Pretext (->) a b) s t a b

-- | @
-- type 'R_ALens'' = 'Simple' 'R_ALens'
-- @
type R_ALens' s a = R_ALens s s a a

-- | When you see this as an argument to a function, it expects an 'R_IndexedLens'
type R_AnIndexedLens i s t a b = R_Optical (Indexed i) (->) (Pretext (Indexed i) a b) s t a b

-- | @
-- type 'R_AnIndexedLens'' = 'Simple' ('R_AnIndexedLens' i)
-- @
type R_AnIndexedLens' i s a  = R_AnIndexedLens i s s a a

--------------------------
-- Constructing R_Lenses
--------------------------

-- | Build a 'R_Lens' from a getter and a setter.
--
-- @
-- 'lens' :: 'Functor' f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
-- @
--
-- >>> s ^. lens getter setter
-- getter s
--
-- >>> s & lens getter setter .~ b
-- setter s b
--
-- >>> s & lens getter setter %~ f
-- setter s (f (getter s))
--
-- @
-- 'lens' :: (s -> a) -> (s -> a -> s) -> 'R_Lens'' s a
-- @
r_lens :: (s -> a) -> (s -> b -> t) -> R_Lens s t a b
r_lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE r_lens #-}

-- | Build a lens from the van Laarhoven representation.
vlLens :: R_Lens s t a b -> Lens s t a b
vlLens = Optic
{-# INLINE vlLens #-}

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt = vlLens (\f s -> sbt s <$> f (sa s))
{-# INLINE lens #-}

-- | Explicitly cast an optic to a lens.
toLens :: AsLens o => Optic o s t a b -> Lens s t a b
toLens = sub
{-# INLINE toLens #-}

-- | Build an 'IndexedLens' from a 'Control.R_Lens.R_Getter.R_Getter' and
-- a 'Control.R_Lens.R_Setter.R_Setter'.
ilens :: (s -> (i, a)) -> (s -> b -> t) -> IndexedLens i s t a b
ilens sia sbt = Optic (\iafb s -> sbt s <$> uncurry (indexed iafb) (sia s))
{-# INLINE ilens #-}

-- | Build an index-preserving 'R_Lens' from a 'Control.R_Lens.R_Getter.R_Getter' and a
-- 'Control.R_Lens.R_Setter.R_Setter'.
iplens :: (s -> a) -> (s -> b -> t) -> R_IndexPreservingLens s t a b
iplens sa sbt pafb = cotabulate $ \ws -> sbt (extract ws) <$> cosieve pafb (sa <$> ws)
{-# INLINE iplens #-}

-- | This can be used to chain lens operations using @op=@ syntax
-- rather than @op~@ syntax for simple non-type-changing cases.
--
-- >>> (10,20) & _1 .~ 30 & _2 .~ 40
-- (30,40)
--
-- >>> (10,20) &~ do _1 .= 30; _2 .= 40
-- (30,40)
--
-- This does not support type-changing assignment, /e.g./
--
-- >>> (10,20) & _1 .~ "hello"
-- ("hello",20)
(&~) :: s -> State s a -> s
s &~ l = execState l s
{-# INLINE (&~) #-}

-- | ('%%~') can be used in one of two scenarios:
--
-- When applied to a 'R_Lens', it can edit the target of the 'R_Lens' in a
-- structure, extracting a functorial result.
--
-- When applied to a 'R_Traversal', it can edit the
-- targets of the traversals, extracting an applicative summary of its
-- actions.
--
-- >>> [66,97,116,109,97,110] & each %%~ \a -> ("na", chr a)
-- ("nananananana","Batman")
--
-- For all that the definition of this combinator is just:
--
-- @
-- ('%%~') ≡ 'id'
-- @
--
-- It may be beneficial to think about it as if it had these even more
-- restricted types, however:
--
-- @
-- ('%%~') :: 'Functor' f =>     'Control.R_Lens.R_Iso.R_Iso' s t a b       -> (a -> f b) -> s -> f t
-- ('%%~') :: 'Functor' f =>     'R_Lens' s t a b      -> (a -> f b) -> s -> f t
-- ('%%~') :: 'Applicative' f => 'Control.R_Lens.R_Traversal.R_Traversal' s t a b -> (a -> f b) -> s -> f t
-- @
--
-- When applied to a 'R_Traversal', it can edit the
-- targets of the traversals, extracting a supplemental monoidal summary
-- of its actions, by choosing @f = ((,) m)@
--
-- @
-- ('%%~') ::             'Control.R_Lens.R_Iso.R_Iso' s t a b       -> (a -> (r, b)) -> s -> (r, t)
-- ('%%~') ::             'R_Lens' s t a b      -> (a -> (r, b)) -> s -> (r, t)
-- ('%%~') :: 'Monoid' m => 'Control.R_Lens.R_Traversal.R_Traversal' s t a b -> (a -> (m, b)) -> s -> (m, t)
-- @
(%%~) :: R_LensLike f s t a b -> (a -> f b) -> s -> f t
(%%~) = id
{-# INLINE (%%~) #-}

-- | Modify the target of a 'R_Lens' in the current state returning some extra
-- information of type @r@ or modify all targets of a
-- 'Control.R_Lens.R_Traversal.R_Traversal' in the current state, extracting extra
-- information of type @r@ and return a monoidal summary of the changes.
--
-- >>> runState (_1 %%= \x -> (f x, g x)) (a,b)
-- (f a,(g a,b))
--
-- @
-- ('%%=') ≡ ('state' '.')
-- @
--
-- It may be useful to think of ('%%='), instead, as having either of the
-- following more restricted type signatures:
--
-- @
-- ('%%=') :: 'MonadState' s m             => 'Control.R_Lens.R_Iso.R_Iso' s s a b       -> (a -> (r, b)) -> m r
-- ('%%=') :: 'MonadState' s m             => 'R_Lens' s s a b      -> (a -> (r, b)) -> m r
-- ('%%=') :: ('MonadState' s m, 'Monoid' r) => 'Control.R_Lens.R_Traversal.R_Traversal' s s a b -> (a -> (r, b)) -> m r
-- @
(%%=) :: MonadState s m => R_Over p ((,) r) s s a b -> p a (r, b) -> m r
#if MIN_VERSION_mtl(2,1,1)
l %%= f = State.state (l f)
#else
l %%= f = do
  (r, s) <- State.gets (l f)
  State.put s
  return r
#endif
{-# INLINE (%%=) #-}

-------------------------------------------------------------------------------
-- General Purpose Combinators
-------------------------------------------------------------------------------


#if !(MIN_VERSION_base(4,8,0))
-- | Passes the result of the left side to the function on the right side (forward pipe operator).
--
-- This is the flipped version of ('$'), which is more common in languages like F# as (@|>@) where it is needed
-- for inference. Here it is supplied for notational convenience and given a precedence that allows it
-- to be nested inside uses of ('$').
--
-- >>> a & f
-- f a
--
-- >>> "hello" & length & succ
-- 6
--
-- This combinator is commonly used when applying multiple 'R_Lens' operations in sequence.
--
-- >>> ("hello","world") & _1.element 0 .~ 'j' & _1.element 4 .~ 'y'
-- ("jelly","world")
--
-- This reads somewhat similar to:
--
-- >>> flip execState ("hello","world") $ do _1.element 0 .= 'j'; _1.element 4 .= 'y'
-- ("jelly","world")
(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}
infixl 1 &
#endif

#if !(MIN_VERSION_base(4,11,0))
-- | Infix flipped 'fmap'.
--
-- @
-- ('<&>') = 'flip' 'fmap'
-- @
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}
infixl 1 <&>
#endif

-- | This is convenient to 'flip' argument order of composite functions defined as:
--
-- @
-- fab ?? a = fmap ($ a) fab
-- @
--
-- For the 'Functor' instance @f = ((->) r)@ you can reason about this function as if the definition was @('??') ≡ 'flip'@:
--
-- >>> (h ?? x) a
-- h a x
--
-- >>> execState ?? [] $ modify (1:)
-- [1]
--
-- >>> over _2 ?? ("hello","world") $ length
-- ("hello",5)
--
-- >>> over ?? length ?? ("hello","world") $ _2
-- ("hello",5)
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}

-------------------------------------------------------------------------------
-- Common R_Lenses
-------------------------------------------------------------------------------

-- | Lift a 'R_Lens' so it can run under a function (or other corepresentable profunctor).
--
-- @
-- 'inside' :: 'R_Lens' s t a b -> 'R_Lens' (e -> s) (e -> t) (e -> a) (e -> b)
-- @
--
--
-- >>> (\x -> (x-1,x+1)) ^. inside _1 $ 5
-- 4
--
-- >>> runState (modify (1:) >> modify (2:)) ^. (inside _2) $ []
-- [2,1]
inside :: Corepresentable p => R_ALens s t a b -> R_Lens (p e s) (p e t) (p e a) (p e b)
inside l f es = o <$> f i where
  i = cotabulate $ \ e -> ipos $ l sell (cosieve es e)
  o ea = cotabulate $ \ e -> ipeek (cosieve ea e) $ l sell (cosieve es e)
{-# INLINE inside #-}

{-
-- | Lift a 'R_Lens' so it can run under a function (or any other corepresentable functor).
insideF :: F.Representable f => R_ALens s t a b -> R_Lens (f s) (f t) (f a) (f b)
insideF l f es = o <$> f i where
  i = F.tabulate $ \e -> ipos $ l sell (F.index es e)
  o ea = F.tabulate $ \ e -> ipeek (F.index ea e) $ l sell (F.index es e)
{-# INLINE inside #-}
-}

-- | Merge two lenses, getters, setters, folds or traversals.
--
-- @
-- 'chosen' ≡ 'choosing' 'id' 'id'
-- @
--
-- @
-- 'choosing' :: 'Control.R_Lens.R_Getter.R_Getter' s a     -> 'Control.R_Lens.R_Getter.R_Getter' s' a     -> 'Control.R_Lens.R_Getter.R_Getter' ('Either' s s') a
-- 'choosing' :: 'Control.R_Lens.R_Fold.R_Fold' s a       -> 'Control.R_Lens.R_Fold.R_Fold' s' a       -> 'Control.R_Lens.R_Fold.R_Fold' ('Either' s s') a
-- 'choosing' :: 'R_Lens'' s a      -> 'R_Lens'' s' a      -> 'R_Lens'' ('Either' s s') a
-- 'choosing' :: 'Control.R_Lens.R_Traversal.R_Traversal'' s a -> 'Control.R_Lens.R_Traversal.R_Traversal'' s' a -> 'Control.R_Lens.R_Traversal.R_Traversal'' ('Either' s s') a
-- 'choosing' :: 'Control.R_Lens.R_Setter.R_Setter'' s a    -> 'Control.R_Lens.R_Setter.R_Setter'' s' a    -> 'Control.R_Lens.R_Setter.R_Setter'' ('Either' s s') a
-- @
choosing :: Functor f
       => R_LensLike f s t a b
       -> R_LensLike f s' t' a b
       -> R_LensLike f (Either s s') (Either t t') a b
choosing l _ f (Left a)   = Left <$> l f a
choosing _ r f (Right a') = Right <$> r f a'
{-# INLINE choosing #-}

-- | This is a 'R_Lens' that updates either side of an 'Either', where both sides have the same type.
--
-- @
-- 'chosen' ≡ 'choosing' 'id' 'id'
-- @
--
-- >>> Left a^.chosen
-- a
--
-- >>> Right a^.chosen
-- a
--
-- >>> Right "hello"^.chosen
-- "hello"
--
-- >>> Right a & chosen *~ b
-- Right (a * b)
--
-- @
-- 'chosen' :: 'R_Lens' ('Either' a a) ('Either' b b) a b
-- 'chosen' f ('Left' a)  = 'Left' '<$>' f a
-- 'chosen' f ('Right' a) = 'Right' '<$>' f a
-- @
chosen :: R_IndexPreservingLens (Either a a) (Either b b) a b
chosen pafb = cotabulate $ \weaa -> cosieve (either id id `lmap` pafb) weaa <&> \b -> case extract weaa of
  Left _  -> Left  b
  Right _ -> Right b
{-# INLINE chosen #-}

-- | 'alongside' makes a 'R_Lens' from two other lenses or a 'R_Getter' from two other getters
-- by executing them on their respective halves of a product.
--
-- >>> (Left a, Right b)^.alongside chosen chosen
-- (a,b)
--
-- >>> (Left a, Right b) & alongside chosen chosen .~ (c,d)
-- (Left c,Right d)
--
-- @
-- 'alongside' :: 'R_Lens'   s t a b -> 'R_Lens'   s' t' a' b' -> 'R_Lens'   (s,s') (t,t') (a,a') (b,b')
-- 'alongside' :: 'R_Getter' s t a b -> 'R_Getter' s' t' a' b' -> 'R_Getter' (s,s') (t,t') (a,a') (b,b')
-- @
alongside :: R_LensLike (AlongsideLeft f b') s  t  a  b
          -> R_LensLike (AlongsideRight f t) s' t' a' b'
          -> R_LensLike f (s, s') (t, t') (a, a') (b, b')
alongside l1 l2 f (a1, a2)
  = getAlongsideRight $ l2 ?? a2 $ \b2 -> AlongsideRight
  $ getAlongsideLeft  $ l1 ?? a1 $ \b1 -> AlongsideLeft
  $ f (b1,b2)
{-# INLINE alongside #-}

-- | This 'R_Lens' lets you 'view' the current 'pos' of any indexed
-- store comonad and 'seek' to a new position. This reduces the API
-- for working these instances to a single 'R_Lens'.
--
-- @
-- 'ipos' w ≡ w 'Control.R_Lens.R_Getter.^.' 'locus'
-- 'iseek' s w ≡ w '&' 'locus' 'Control.R_Lens.R_Setter..~' s
-- 'iseeks' f w ≡ w '&' 'locus' 'Control.R_Lens.R_Setter.%~' f
-- @
--
-- @
-- 'locus' :: 'R_Lens'' ('Context'' a s) a
-- 'locus' :: 'Conjoined' p => 'R_Lens'' ('Pretext'' p a s) a
-- 'locus' :: 'Conjoined' p => 'R_Lens'' ('PretextT'' p g a s) a
-- @
locus :: IndexedComonadStore p => R_Lens (p a c s) (p b c s) a b
locus f w = (`iseek` w) <$> f (ipos w)
{-# INLINE locus #-}

-------------------------------------------------------------------------------
-- Cloning R_Lenses
-------------------------------------------------------------------------------

-- | Cloning a 'R_Lens' is one way to make sure you aren't given
-- something weaker, such as a 'Control.R_Lens.R_Traversal.R_Traversal' and can be
-- used as a way to pass around lenses that have to be monomorphic in @f@.
--
-- Note: This only accepts a proper 'R_Lens'.
--
-- >>> let example l x = set (cloneR_Lens l) (x^.cloneR_Lens l + 1) x in example _2 ("hello",1,"you")
-- ("hello",2,"you")
cloneR_Lens :: R_ALens s t a b -> R_Lens s t a b
cloneR_Lens l afb s = runPretext (l sell s) afb
{-# INLINE cloneR_Lens #-}

-- | Clone a 'R_Lens' as an 'IndexedPreservingR_Lens' that just passes through whatever
-- index is on any 'R_IndexedLens', 'R_IndexedFold', 'R_IndexedGetter' or  'R_IndexedTraversal' it is composed with.
cloneR_IndexPreservingLens :: R_ALens s t a b -> R_IndexPreservingLens s t a b
cloneR_IndexPreservingLens l pafb = cotabulate $ \ws -> runPretext (l sell (extract ws)) $ \a -> cosieve pafb (a <$ ws)
{-# INLINE cloneR_IndexPreservingLens #-}

-- | Clone an 'R_IndexedLens' as an 'R_IndexedLens' with the same index.
cloneR_IndexedLens :: R_AnIndexedLens i s t a b -> R_IndexedLens i s t a b
cloneR_IndexedLens l f s = runPretext (l sell s) (Indexed (indexed f))
{-# INLINE cloneR_IndexedLens #-}

-------------------------------------------------------------------------------
-- Setting and Remembering
-------------------------------------------------------------------------------

-- | Modify the target of a 'R_Lens' and return the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.%~') is more flexible.
--
-- @
-- ('<%~') ::             'R_Lens' s t a b      -> (a -> b) -> s -> (b, t)
-- ('<%~') ::             'Control.R_Lens.R_Iso.R_Iso' s t a b       -> (a -> b) -> s -> (b, t)
-- ('<%~') :: 'Monoid' b => 'Control.R_Lens.R_Traversal.R_Traversal' s t a b -> (a -> b) -> s -> (b, t)
-- @
(<%~) :: R_LensLike ((,) b) s t a b -> (a -> b) -> s -> (b, t)
l <%~ f = l $ (\t -> (t, t)) . f
{-# INLINE (<%~) #-}

-- | Increment the target of a numerically valued 'R_Lens' and return the result.
--
-- When you do not need the result of the addition, ('Control.R_Lens.R_Setter.+~') is more flexible.
--
-- @
-- ('<+~') :: 'Num' a => 'R_Lens'' s a -> a -> s -> (a, s)
-- ('<+~') :: 'Num' a => 'Control.R_Lens.R_Iso.R_Iso'' s a  -> a -> s -> (a, s)
-- @
(<+~) :: Num a => R_LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <+~ a = l <%~ (+ a)
{-# INLINE (<+~) #-}

-- | Decrement the target of a numerically valued 'R_Lens' and return the result.
--
-- When you do not need the result of the subtraction, ('Control.R_Lens.R_Setter.-~') is more flexible.
--
-- @
-- ('<-~') :: 'Num' a => 'R_Lens'' s a -> a -> s -> (a, s)
-- ('<-~') :: 'Num' a => 'Control.R_Lens.R_Iso.R_Iso'' s a  -> a -> s -> (a, s)
-- @
(<-~) :: Num a => R_LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <-~ a = l <%~ subtract a
{-# INLINE (<-~) #-}

-- | Multiply the target of a numerically valued 'R_Lens' and return the result.
--
-- When you do not need the result of the multiplication, ('Control.R_Lens.R_Setter.*~') is more
-- flexible.
--
-- @
-- ('<*~') :: 'Num' a => 'R_Lens'' s a -> a -> s -> (a, s)
-- ('<*~') :: 'Num' a => 'Control.R_Lens.R_Iso.R_Iso''  s a -> a -> s -> (a, s)
-- @
(<*~) :: Num a => R_LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <*~ a = l <%~ (* a)
{-# INLINE (<*~) #-}

-- | Divide the target of a fractionally valued 'R_Lens' and return the result.
--
-- When you do not need the result of the division, ('Control.R_Lens.R_Setter.//~') is more flexible.
--
-- @
-- ('<//~') :: 'Fractional' a => 'R_Lens'' s a -> a -> s -> (a, s)
-- ('<//~') :: 'Fractional' a => 'Control.R_Lens.R_Iso.R_Iso''  s a -> a -> s -> (a, s)
-- @
(<//~) :: Fractional a => R_LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <//~ a = l <%~ (/ a)
{-# INLINE (<//~) #-}

-- | Raise the target of a numerically valued 'R_Lens' to a non-negative
-- 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.^~') is more flexible.
--
-- @
-- ('<^~') :: ('Num' a, 'Integral' e) => 'R_Lens'' s a -> e -> s -> (a, s)
-- ('<^~') :: ('Num' a, 'Integral' e) => 'Control.R_Lens.R_Iso.R_Iso'' s a -> e -> s -> (a, s)
-- @
(<^~) :: (Num a, Integral e) => R_LensLike ((,)a) s t a a -> e -> s -> (a, t)
l <^~ e = l <%~ (^ e)
{-# INLINE (<^~) #-}

-- | Raise the target of a fractionally valued 'R_Lens' to an 'Integral' power
-- and return the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.^^~') is more flexible.
--
-- @
-- ('<^^~') :: ('Fractional' a, 'Integral' e) => 'R_Lens'' s a -> e -> s -> (a, s)
-- ('<^^~') :: ('Fractional' a, 'Integral' e) => 'Control.R_Lens.R_Iso.R_Iso'' s a -> e -> s -> (a, s)
-- @
(<^^~) :: (Fractional a, Integral e) => R_LensLike ((,)a) s t a a -> e -> s -> (a, t)
l <^^~ e = l <%~ (^^ e)
{-# INLINE (<^^~) #-}

-- | Raise the target of a floating-point valued 'R_Lens' to an arbitrary power
-- and return the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.**~') is more flexible.
--
-- @
-- ('<**~') :: 'Floating' a => 'R_Lens'' s a -> a -> s -> (a, s)
-- ('<**~') :: 'Floating' a => 'Control.R_Lens.R_Iso.R_Iso'' s a  -> a -> s -> (a, s)
-- @
(<**~) :: Floating a => R_LensLike ((,)a) s t a a -> a -> s -> (a, t)
l <**~ a = l <%~ (** a)
{-# INLINE (<**~) #-}

-- | Logically '||' a Boolean valued 'R_Lens' and return the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.||~') is more flexible.
--
-- @
-- ('<||~') :: 'R_Lens'' s 'Bool' -> 'Bool' -> s -> ('Bool', s)
-- ('<||~') :: 'Control.R_Lens.R_Iso.R_Iso'' s 'Bool'  -> 'Bool' -> s -> ('Bool', s)
-- @
(<||~) :: R_LensLike ((,)Bool) s t Bool Bool -> Bool -> s -> (Bool, t)
l <||~ b = l <%~ (|| b)
{-# INLINE (<||~) #-}

-- | Logically '&&' a Boolean valued 'R_Lens' and return the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.&&~') is more flexible.
--
-- @
-- ('<&&~') :: 'R_Lens'' s 'Bool' -> 'Bool' -> s -> ('Bool', s)
-- ('<&&~') :: 'Control.R_Lens.R_Iso.R_Iso'' s 'Bool'  -> 'Bool' -> s -> ('Bool', s)
-- @
(<&&~) :: R_LensLike ((,)Bool) s t Bool Bool -> Bool -> s -> (Bool, t)
l <&&~ b = l <%~ (&& b)
{-# INLINE (<&&~) #-}

-- | Modify the target of a 'R_Lens', but return the old value.
--
-- When you do not need the old value, ('Control.R_Lens.R_Setter.%~') is more flexible.
--
-- @
-- ('<<%~') ::             'R_Lens' s t a b      -> (a -> b) -> s -> (a, t)
-- ('<<%~') ::             'Control.R_Lens.R_Iso.R_Iso' s t a b       -> (a -> b) -> s -> (a, t)
-- ('<<%~') :: 'Monoid' a => 'Control.R_Lens.R_Traversal.R_Traversal' s t a b -> (a -> b) -> s -> (a, t)
-- @
(<<%~) :: R_LensLike ((,)a) s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l = l . lmap (\a -> (a, a)) . second'
{-# INLINE (<<%~) #-}

-- | Replace the target of a 'R_Lens', but return the old value.
--
-- When you do not need the old value, ('Control.R_Lens.R_Setter..~') is more flexible.
--
-- @
-- ('<<.~') ::             'R_Lens' s t a b      -> b -> s -> (a, t)
-- ('<<.~') ::             'Control.R_Lens.R_Iso.R_Iso' s t a b       -> b -> s -> (a, t)
-- ('<<.~') :: 'Monoid' a => 'Control.R_Lens.R_Traversal.R_Traversal' s t a b -> b -> s -> (a, t)
-- @
(<<.~) :: R_LensLike ((,)a) s t a b -> b -> s -> (a, t)
l <<.~ b = l $ \a -> (a, b)
{-# INLINE (<<.~) #-}

-- | Replace the target of a 'R_Lens' with a 'Just' value, but return the old value.
--
-- If you do not need the old value ('Control.R_Lens.R_Setter.?~') is more flexible.
--
-- >>> import Data.Map as Map
-- >>> _2.at "hello" <<?~ "world" $ (42,Map.fromList [("goodnight","gracie")])
-- (Nothing,(42,fromList [("goodnight","gracie"),("hello","world")]))
--
-- @
-- ('<<?~') :: 'R_Iso' s t a ('Maybe' b)       -> b -> s -> (a, t)
-- ('<<?~') :: 'R_Lens' s t a ('Maybe' b)      -> b -> s -> (a, t)
-- ('<<?~') :: 'R_Traversal' s t a ('Maybe' b) -> b -> s -> (a, t)
-- @
(<<?~) :: R_LensLike ((,)a) s t a (Maybe b) -> b -> s -> (a, t)
l <<?~ b = l <<.~ Just b
{-# INLINE (<<?~) #-}

-- | Increment the target of a numerically valued 'R_Lens' and return the old value.
--
-- When you do not need the old value, ('Control.R_Lens.R_Setter.+~') is more flexible.
--
-- >>> (a,b) & _1 <<+~ c
-- (a,(a + c,b))
--
-- >>> (a,b) & _2 <<+~ c
-- (b,(a,b + c))
--
-- @
-- ('<<+~') :: 'Num' a => 'R_Lens'' s a -> a -> s -> (a, s)
-- ('<<+~') :: 'Num' a => 'R_Iso'' s a -> a -> s -> (a, s)
-- @
(<<+~) :: Num a => R_LensLike' ((,) a) s a -> a -> s -> (a, s)
l <<+~ b = l $ \a -> (a, a + b)
{-# INLINE (<<+~) #-}

-- | Decrement the target of a numerically valued 'R_Lens' and return the old value.
--
-- When you do not need the old value, ('Control.R_Lens.R_Setter.-~') is more flexible.
--
-- >>> (a,b) & _1 <<-~ c
-- (a,(a - c,b))
--
-- >>> (a,b) & _2 <<-~ c
-- (b,(a,b - c))
--
-- @
-- ('<<-~') :: 'Num' a => 'R_Lens'' s a -> a -> s -> (a, s)
-- ('<<-~') :: 'Num' a => 'R_Iso'' s a -> a -> s -> (a, s)
-- @
(<<-~) :: Num a => R_LensLike' ((,) a) s a -> a -> s -> (a, s)
l <<-~ b = l $ \a -> (a, a - b)
{-# INLINE (<<-~) #-}

-- | Multiply the target of a numerically valued 'R_Lens' and return the old value.
--
-- When you do not need the old value, ('Control.R_Lens.R_Setter.-~') is more flexible.
--
-- >>> (a,b) & _1 <<*~ c
-- (a,(a * c,b))
--
-- >>> (a,b) & _2 <<*~ c
-- (b,(a,b * c))
--
-- @
-- ('<<*~') :: 'Num' a => 'R_Lens'' s a -> a -> s -> (a, s)
-- ('<<*~') :: 'Num' a => 'R_Iso'' s a -> a -> s -> (a, s)
-- @
(<<*~) :: Num a => R_LensLike' ((,) a) s a -> a -> s -> (a, s)
l <<*~ b = l $ \a -> (a, a * b)
{-# INLINE (<<*~) #-}

-- | Divide the target of a numerically valued 'R_Lens' and return the old value.
--
-- When you do not need the old value, ('Control.R_Lens.R_Setter.//~') is more flexible.
--
-- >>> (a,b) & _1 <<//~ c
-- (a,(a / c,b))
--
-- >>> ("Hawaii",10) & _2 <<//~ 2
-- (10.0,("Hawaii",5.0))
--
-- @
-- ('<<//~') :: Fractional a => 'R_Lens'' s a -> a -> s -> (a, s)
-- ('<<//~') :: Fractional a => 'R_Iso'' s a -> a -> s -> (a, s)
-- @
(<<//~) :: Fractional a => R_LensLike' ((,) a) s a -> a -> s -> (a, s)
l <<//~ b = l $ \a -> (a, a / b)
{-# INLINE (<<//~) #-}

-- | Raise the target of a numerically valued 'R_Lens' to a non-negative power and return the old value.
--
-- When you do not need the old value, ('Control.R_Lens.R_Setter.^~') is more flexible.
--
-- @
-- ('<<^~') :: ('Num' a, 'Integral' e) => 'R_Lens'' s a -> e -> s -> (a, s)
-- ('<<^~') :: ('Num' a, 'Integral' e) => 'R_Iso'' s a -> e -> s -> (a, s)
-- @
(<<^~) :: (Num a, Integral e) => R_LensLike' ((,) a) s a -> e -> s -> (a, s)
l <<^~ e = l $ \a -> (a, a ^ e)
{-# INLINE (<<^~) #-}

-- | Raise the target of a fractionally valued 'R_Lens' to an integral power and return the old value.
--
-- When you do not need the old value, ('Control.R_Lens.R_Setter.^^~') is more flexible.
--
-- @
-- ('<<^^~') :: ('Fractional' a, 'Integral' e) => 'R_Lens'' s a -> e -> s -> (a, s)
-- ('<<^^~') :: ('Fractional' a, 'Integral' e) => 'R_Iso'' s a -> e -> S -> (a, s)
-- @
(<<^^~) :: (Fractional a, Integral e) => R_LensLike' ((,) a) s a -> e -> s -> (a, s)
l <<^^~ e = l $ \a -> (a, a ^^ e)
{-# INLINE (<<^^~) #-}

-- | Raise the target of a floating-point valued 'R_Lens' to an arbitrary power and return the old value.
--
-- When you do not need the old value, ('Control.R_Lens.R_Setter.**~') is more flexible.
--
-- >>> (a,b) & _1 <<**~ c
-- (a,(a**c,b))
--
-- >>> (a,b) & _2 <<**~ c
-- (b,(a,b**c))
--
-- @
-- ('<<**~') :: 'Floating' a => 'R_Lens'' s a -> a -> s -> (a, s)
-- ('<<**~') :: 'Floating' a => 'R_Iso'' s a -> a -> s -> (a, s)
-- @
(<<**~) :: Floating a => R_LensLike' ((,) a) s a -> a -> s -> (a, s)
l <<**~ e = l $ \a -> (a, a ** e)
{-# INLINE (<<**~) #-}

-- | Logically '||' the target of a 'Bool'-valued 'R_Lens' and return the old value.
--
-- When you do not need the old value, ('Control.R_Lens.R_Setter.||~') is more flexible.
--
-- >>> (False,6) & _1 <<||~ True
-- (False,(True,6))
--
-- >>> ("hello",True) & _2 <<||~ False
-- (True,("hello",True))
--
-- @
-- ('<<||~') :: 'R_Lens'' s 'Bool' -> 'Bool' -> s -> ('Bool', s)
-- ('<<||~') :: 'R_Iso'' s 'Bool' -> 'Bool' -> s -> ('Bool', s)
-- @
(<<||~) :: R_LensLike' ((,) Bool) s Bool -> Bool -> s -> (Bool, s)
l <<||~ b = l $ \a -> (a, b || a)
{-# INLINE (<<||~) #-}

-- | Logically '&&' the target of a 'Bool'-valued 'R_Lens' and return the old value.
--
-- When you do not need the old value, ('Control.R_Lens.R_Setter.&&~') is more flexible.
--
-- >>> (False,6) & _1 <<&&~ True
-- (False,(False,6))
--
-- >>> ("hello",True) & _2 <<&&~ False
-- (True,("hello",False))
--
-- @
-- ('<<&&~') :: 'R_Lens'' s Bool -> Bool -> s -> (Bool, s)
-- ('<<&&~') :: 'R_Iso'' s Bool -> Bool -> s -> (Bool, s)
-- @
(<<&&~) :: R_LensLike' ((,) Bool) s Bool -> Bool -> s -> (Bool, s)
l <<&&~ b = l $ \a -> (a, b && a)
{-# INLINE (<<&&~) #-}

-- | Modify the target of a monoidally valued 'R_Lens' by 'mappend'ing a new value and return the old value.
--
-- When you do not need the old value, ('Control.R_Lens.R_Setter.<>~') is more flexible.
--
-- >>> (Sum a,b) & _1 <<<>~ Sum c
-- (Sum {getSum = a},(Sum {getSum = a + c},b))
--
-- >>> _2 <<<>~ ", 007" $ ("James", "Bond")
-- ("Bond",("James","Bond, 007"))
--
-- @
-- ('<<<>~') :: 'Monoid' r => 'R_Lens'' s r -> r -> s -> (r, s)
-- ('<<<>~') :: 'Monoid' r => 'R_Iso'' s r -> r -> s -> (r, s)
-- @
(<<<>~) :: Monoid r => R_LensLike' ((,) r) s r -> r -> s -> (r, s)
l <<<>~ b = l $ \a -> (a, a `mappend` b)
{-# INLINE (<<<>~) #-}

-------------------------------------------------------------------------------
-- Setting and Remembering State
-------------------------------------------------------------------------------

-- | Modify the target of a 'R_Lens' into your 'Monad''s state by a user supplied
-- function and return the result.
--
-- When applied to a 'Control.R_Lens.R_Traversal.R_Traversal', it this will return a monoidal summary of all of the intermediate
-- results.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.%=') is more flexible.
--
-- @
-- ('<%=') :: 'MonadState' s m             => 'R_Lens'' s a      -> (a -> a) -> m a
-- ('<%=') :: 'MonadState' s m             => 'Control.R_Lens.R_Iso.R_Iso'' s a       -> (a -> a) -> m a
-- ('<%=') :: ('MonadState' s m, 'Monoid' a) => 'Control.R_Lens.R_Traversal.R_Traversal'' s a -> (a -> a) -> m a
-- @
(<%=) :: MonadState s m => R_LensLike ((,)b) s s a b -> (a -> b) -> m b
l <%= f = l %%= (\b -> (b, b)) . f
{-# INLINE (<%=) #-}


-- | Add to the target of a numerically valued 'R_Lens' into your 'Monad''s state
-- and return the result.
--
-- When you do not need the result of the addition, ('Control.R_Lens.R_Setter.+=') is more
-- flexible.
--
-- @
-- ('<+=') :: ('MonadState' s m, 'Num' a) => 'R_Lens'' s a -> a -> m a
-- ('<+=') :: ('MonadState' s m, 'Num' a) => 'Control.R_Lens.R_Iso.R_Iso'' s a -> a -> m a
-- @
(<+=) :: (MonadState s m, Num a) => R_LensLike' ((,)a) s a -> a -> m a
l <+= a = l <%= (+ a)
{-# INLINE (<+=) #-}

-- | Subtract from the target of a numerically valued 'R_Lens' into your 'Monad''s
-- state and return the result.
--
-- When you do not need the result of the subtraction, ('Control.R_Lens.R_Setter.-=') is more
-- flexible.
--
-- @
-- ('<-=') :: ('MonadState' s m, 'Num' a) => 'R_Lens'' s a -> a -> m a
-- ('<-=') :: ('MonadState' s m, 'Num' a) => 'Control.R_Lens.R_Iso.R_Iso'' s a -> a -> m a
-- @
(<-=) :: (MonadState s m, Num a) => R_LensLike' ((,)a) s a -> a -> m a
l <-= a = l <%= subtract a
{-# INLINE (<-=) #-}

-- | Multiply the target of a numerically valued 'R_Lens' into your 'Monad''s
-- state and return the result.
--
-- When you do not need the result of the multiplication, ('Control.R_Lens.R_Setter.*=') is more
-- flexible.
--
-- @
-- ('<*=') :: ('MonadState' s m, 'Num' a) => 'R_Lens'' s a -> a -> m a
-- ('<*=') :: ('MonadState' s m, 'Num' a) => 'Control.R_Lens.R_Iso.R_Iso'' s a -> a -> m a
-- @
(<*=) :: (MonadState s m, Num a) => R_LensLike' ((,)a) s a -> a -> m a
l <*= a = l <%= (* a)
{-# INLINE (<*=) #-}

-- | Divide the target of a fractionally valued 'R_Lens' into your 'Monad''s state
-- and return the result.
--
-- When you do not need the result of the division, ('Control.R_Lens.R_Setter.//=') is more flexible.
--
-- @
-- ('<//=') :: ('MonadState' s m, 'Fractional' a) => 'R_Lens'' s a -> a -> m a
-- ('<//=') :: ('MonadState' s m, 'Fractional' a) => 'Control.R_Lens.R_Iso.R_Iso'' s a -> a -> m a
-- @
(<//=) :: (MonadState s m, Fractional a) => R_LensLike' ((,)a) s a -> a -> m a
l <//= a = l <%= (/ a)
{-# INLINE (<//=) #-}

-- | Raise the target of a numerically valued 'R_Lens' into your 'Monad''s state
-- to a non-negative 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.^=') is more flexible.
--
-- @
-- ('<^=') :: ('MonadState' s m, 'Num' a, 'Integral' e) => 'R_Lens'' s a -> e -> m a
-- ('<^=') :: ('MonadState' s m, 'Num' a, 'Integral' e) => 'Control.R_Lens.R_Iso.R_Iso'' s a -> e -> m a
-- @
(<^=) :: (MonadState s m, Num a, Integral e) => R_LensLike' ((,)a) s a -> e -> m a
l <^= e = l <%= (^ e)
{-# INLINE (<^=) #-}

-- | Raise the target of a fractionally valued 'R_Lens' into your 'Monad''s state
-- to an 'Integral' power and return the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.^^=') is more flexible.
--
-- @
-- ('<^^=') :: ('MonadState' s m, 'Fractional' b, 'Integral' e) => 'R_Lens'' s a -> e -> m a
-- ('<^^=') :: ('MonadState' s m, 'Fractional' b, 'Integral' e) => 'Control.R_Lens.R_Iso.R_Iso'' s a  -> e -> m a
-- @
(<^^=) :: (MonadState s m, Fractional a, Integral e) => R_LensLike' ((,)a) s a -> e -> m a
l <^^= e = l <%= (^^ e)
{-# INLINE (<^^=) #-}

-- | Raise the target of a floating-point valued 'R_Lens' into your 'Monad''s
-- state to an arbitrary power and return the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.**=') is more flexible.
--
-- @
-- ('<**=') :: ('MonadState' s m, 'Floating' a) => 'R_Lens'' s a -> a -> m a
-- ('<**=') :: ('MonadState' s m, 'Floating' a) => 'Control.R_Lens.R_Iso.R_Iso'' s a -> a -> m a
-- @
(<**=) :: (MonadState s m, Floating a) => R_LensLike' ((,)a) s a -> a -> m a
l <**= a = l <%= (** a)
{-# INLINE (<**=) #-}

-- | Logically '||' a Boolean valued 'R_Lens' into your 'Monad''s state and return
-- the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.||=') is more flexible.
--
-- @
-- ('<||=') :: 'MonadState' s m => 'R_Lens'' s 'Bool' -> 'Bool' -> m 'Bool'
-- ('<||=') :: 'MonadState' s m => 'Control.R_Lens.R_Iso.R_Iso'' s 'Bool'  -> 'Bool' -> m 'Bool'
-- @
(<||=) :: MonadState s m => R_LensLike' ((,)Bool) s Bool -> Bool -> m Bool
l <||= b = l <%= (|| b)
{-# INLINE (<||=) #-}

-- | Logically '&&' a Boolean valued 'R_Lens' into your 'Monad''s state and return
-- the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.&&=') is more flexible.
--
-- @
-- ('<&&=') :: 'MonadState' s m => 'R_Lens'' s 'Bool' -> 'Bool' -> m 'Bool'
-- ('<&&=') :: 'MonadState' s m => 'Control.R_Lens.R_Iso.R_Iso'' s 'Bool'  -> 'Bool' -> m 'Bool'
-- @
(<&&=) :: MonadState s m => R_LensLike' ((,)Bool) s Bool -> Bool -> m Bool
l <&&= b = l <%= (&& b)
{-# INLINE (<&&=) #-}

-- | Modify the target of a 'R_Lens' into your 'Monad''s state by a user supplied
-- function and return the /old/ value that was replaced.
--
-- When applied to a 'Control.R_Lens.R_Traversal.R_Traversal', this will return a monoidal summary of all of the old values
-- present.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.%=') is more flexible.
--
-- @
-- ('<<%=') :: 'MonadState' s m             => 'R_Lens'' s a      -> (a -> a) -> m a
-- ('<<%=') :: 'MonadState' s m             => 'Control.R_Lens.R_Iso.R_Iso'' s a       -> (a -> a) -> m a
-- ('<<%=') :: ('MonadState' s m, 'Monoid' a) => 'Control.R_Lens.R_Traversal.R_Traversal'' s a -> (a -> a) -> m a
-- @
--
-- @('<<%=') :: 'MonadState' s m => 'R_LensLike' ((,)a) s s a b -> (a -> b) -> m a@
(<<%=) :: (Strong p, MonadState s m) => R_Over p ((,)a) s s a b -> p a b -> m a
l <<%= f = l %%= lmap (\a -> (a,a)) (second' f)
{-# INLINE (<<%=) #-}

-- | Replace the target of a 'R_Lens' into your 'Monad''s state with a user supplied
-- value and return the /old/ value that was replaced.
--
-- When applied to a 'Control.R_Lens.R_Traversal.R_Traversal', this will return a monoidal summary of all of the old values
-- present.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter..=') is more flexible.
--
-- @
-- ('<<.=') :: 'MonadState' s m             => 'R_Lens'' s a      -> a -> m a
-- ('<<.=') :: 'MonadState' s m             => 'Control.R_Lens.R_Iso.R_Iso'' s a       -> a -> m a
-- ('<<.=') :: ('MonadState' s m, 'Monoid' a) => 'Control.R_Lens.R_Traversal.R_Traversal'' s a -> a -> m a
-- @
(<<.=) :: MonadState s m => R_LensLike ((,)a) s s a b -> b -> m a
l <<.= b = l %%= \a -> (a,b)
{-# INLINE (<<.=) #-}

-- | Replace the target of a 'R_Lens' into your 'Monad''s state with 'Just' a user supplied
-- value and return the /old/ value that was replaced.
--
-- When applied to a 'Control.R_Lens.R_Traversal.R_Traversal', this will return a monoidal summary of all of the old values
-- present.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.?=') is more flexible.
--
-- @
-- ('<<?=') :: 'MonadState' s m             => 'R_Lens' s t a (Maybe b)      -> b -> m a
-- ('<<?=') :: 'MonadState' s m             => 'Control.R_Lens.R_Iso.R_Iso' s t a (Maybe b)       -> b -> m a
-- ('<<?=') :: ('MonadState' s m, 'Monoid' a) => 'Control.R_Lens.R_Traversal.R_Traversal' s t a (Maybe b) -> b -> m a
-- @
(<<?=) :: MonadState s m => R_LensLike ((,)a) s s a (Maybe b) -> b -> m a
l <<?= b = l <<.= Just b
{-# INLINE (<<?=) #-}

-- | Modify the target of a 'R_Lens' into your 'Monad''s state by adding a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.+=') is more flexible.
--
-- @
-- ('<<+=') :: ('MonadState' s m, 'Num' a) => 'R_Lens'' s a -> a -> m a
-- ('<<+=') :: ('MonadState' s m, 'Num' a) => 'R_Iso'' s a -> a -> m a
-- @
(<<+=) :: (MonadState s m, Num a) => R_LensLike' ((,) a) s a -> a -> m a
l <<+= n = l %%= \a -> (a, a + n)
{-# INLINE (<<+=) #-}

-- | Modify the target of a 'R_Lens' into your 'Monad''s state by subtracting a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.-=') is more flexible.
--
-- @
-- ('<<-=') :: ('MonadState' s m, 'Num' a) => 'R_Lens'' s a -> a -> m a
-- ('<<-=') :: ('MonadState' s m, 'Num' a) => 'R_Iso'' s a -> a -> m a
-- @
(<<-=) :: (MonadState s m, Num a) => R_LensLike' ((,) a) s a -> a -> m a
l <<-= n = l %%= \a -> (a, a - n)
{-# INLINE (<<-=) #-}

-- | Modify the target of a 'R_Lens' into your 'Monad''s state by multipling a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.*=') is more flexible.
--
-- @
-- ('<<*=') :: ('MonadState' s m, 'Num' a) => 'R_Lens'' s a -> a -> m a
-- ('<<*=') :: ('MonadState' s m, 'Num' a) => 'R_Iso'' s a -> a -> m a
-- @
(<<*=) :: (MonadState s m, Num a) => R_LensLike' ((,) a) s a -> a -> m a
l <<*= n = l %%= \a -> (a, a * n)
{-# INLINE (<<*=) #-}

-- | Modify the target of a 'R_Lens' into your 'Monad'\s state by dividing by a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.//=') is more flexible.
--
-- @
-- ('<<//=') :: ('MonadState' s m, 'Fractional' a) => 'R_Lens'' s a -> a -> m a
-- ('<<//=') :: ('MonadState' s m, 'Fractional' a) => 'R_Iso'' s a -> a -> m a
-- @
(<<//=) :: (MonadState s m, Fractional a) => R_LensLike' ((,) a) s a -> a -> m a
l <<//= n = l %%= \a -> (a, a / n)
{-# INLINE (<<//=) #-}

-- | Modify the target of a 'R_Lens' into your 'Monad''s state by raising it by a non-negative power
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.^=') is more flexible.
--
-- @
-- ('<<^=') :: ('MonadState' s m, 'Num' a, 'Integral' e) => 'R_Lens'' s a -> e -> m a
-- ('<<^=') :: ('MonadState' s m, 'Num' a, 'Integral' e) => 'R_Iso'' s a -> a -> m a
-- @
(<<^=) :: (MonadState s m, Num a, Integral e) => R_LensLike' ((,) a) s a -> e -> m a
l <<^= n = l %%= \a -> (a, a ^ n)
{-# INLINE (<<^=) #-}

-- | Modify the target of a 'R_Lens' into your 'Monad''s state by raising it by an integral power
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.^^=') is more flexible.
--
-- @
-- ('<<^^=') :: ('MonadState' s m, 'Fractional' a, 'Integral' e) => 'R_Lens'' s a -> e -> m a
-- ('<<^^=') :: ('MonadState' s m, 'Fractional' a, 'Integral' e) => 'R_Iso'' s a -> e -> m a
-- @
(<<^^=) :: (MonadState s m, Fractional a, Integral e) => R_LensLike' ((,) a) s a -> e -> m a
l <<^^= n = l %%= \a -> (a, a ^^ n)
{-# INLINE (<<^^=) #-}

-- | Modify the target of a 'R_Lens' into your 'Monad''s state by raising it by an arbitrary power
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.**=') is more flexible.
--
-- @
-- ('<<**=') :: ('MonadState' s m, 'Floating' a) => 'R_Lens'' s a -> a -> m a
-- ('<<**=') :: ('MonadState' s m, 'Floating' a) => 'R_Iso'' s a -> a -> m a
-- @
(<<**=) :: (MonadState s m, Floating a) => R_LensLike' ((,) a) s a -> a -> m a
l <<**= n = l %%= \a -> (a, a ** n)
{-# INLINE (<<**=) #-}

-- | Modify the target of a 'R_Lens' into your 'Monad''s state by taking its logical '||' with a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.||=') is more flexible.
--
-- @
-- ('<<||=') :: 'MonadState' s m => 'R_Lens'' s 'Bool' -> 'Bool' -> m 'Bool'
-- ('<<||=') :: 'MonadState' s m => 'R_Iso'' s 'Bool' -> 'Bool' -> m 'Bool'
-- @
(<<||=) :: MonadState s m => R_LensLike' ((,) Bool) s Bool -> Bool -> m Bool
l <<||= b = l %%= \a -> (a, a || b)
{-# INLINE (<<||=) #-}

-- | Modify the target of a 'R_Lens' into your 'Monad''s state by taking its logical '&&' with a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.&&=') is more flexible.
--
-- @
-- ('<<&&=') :: 'MonadState' s m => 'R_Lens'' s 'Bool' -> 'Bool' -> m 'Bool'
-- ('<<&&=') :: 'MonadState' s m => 'R_Iso'' s 'Bool' -> 'Bool' -> m 'Bool'
-- @
(<<&&=) :: MonadState s m => R_LensLike' ((,) Bool) s Bool -> Bool -> m Bool
l <<&&= b = l %%= \a -> (a, a && b)
{-# INLINE (<<&&=) #-}

-- | Modify the target of a 'R_Lens' into your 'Monad''s state by 'mappend'ing a value
-- and return the /old/ value that was replaced.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.<>=') is more flexible.
--
-- @
-- ('<<<>=') :: ('MonadState' s m, 'Monoid' r) => 'R_Lens'' s r -> r -> m r
-- ('<<<>=') :: ('MonadState' s m, 'Monoid' r) => 'R_Iso'' s r -> r -> m r
-- @
(<<<>=) :: (MonadState s m, Monoid r) => R_LensLike' ((,) r) s r -> r -> m r
l <<<>= b = l %%= \a -> (a, a `mappend` b)
{-# INLINE (<<<>=) #-}

-- | Run a monadic action, and set the target of 'R_Lens' to its result.
--
-- @
-- ('<<~') :: 'MonadState' s m => 'Control.R_Lens.R_Iso.R_Iso' s s a b   -> m b -> m b
-- ('<<~') :: 'MonadState' s m => 'R_Lens' s s a b  -> m b -> m b
-- @
--
-- NB: This is limited to taking an actual 'R_Lens' than admitting a 'Control.R_Lens.R_Traversal.R_Traversal' because
-- there are potential loss of state issues otherwise.
(<<~) :: MonadState s m => R_ALens s s a b -> m b -> m b
l <<~ mb = do
  b <- mb
  modify $ \s -> ipeek b (l sell s)
  return b
{-# INLINE (<<~) #-}

-- | 'mappend' a monoidal value onto the end of the target of a 'R_Lens' and
-- return the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.<>~') is more flexible.
(<<>~) :: Monoid m => R_LensLike ((,)m) s t m m -> m -> s -> (m, t)
l <<>~ m = l <%~ (`mappend` m)
{-# INLINE (<<>~) #-}

-- | 'mappend' a monoidal value onto the end of the target of a 'R_Lens' into
-- your 'Monad''s state and return the result.
--
-- When you do not need the result of the operation, ('Control.R_Lens.R_Setter.<>=') is more flexible.
(<<>=) :: (MonadState s m, Monoid r) => R_LensLike' ((,)r) s r -> r -> m r
l <<>= r = l <%= (`mappend` r)
{-# INLINE (<<>=) #-}

------------------------------------------------------------------------------
-- Arrow operators
------------------------------------------------------------------------------

-- | 'Control.R_Lens.R_Setter.over' for Arrows.
--
-- Unlike 'Control.R_Lens.R_Setter.over', 'overA' can't accept a simple
-- 'Control.R_Lens.R_Setter.R_Setter', but requires a full lens, or close
-- enough.
--
-- >>> overA _1 ((+1) *** (+2)) ((1,2),6)
-- ((2,4),6)
--
-- @
-- overA :: Arrow ar => R_Lens s t a b -> ar a b -> ar s t
-- @
overA :: Arrow ar => R_LensLike (Context a b) s t a b -> ar a b -> ar s t
overA l p = arr (\s -> let (Context f a) = l sell s in (f, a))
            >>> second p
            >>> arr (uncurry id)

------------------------------------------------------------------------------
-- Indexed
------------------------------------------------------------------------------

-- | Adjust the target of an 'R_IndexedLens' returning the intermediate result, or
-- adjust all of the targets of an 'Control.R_Lens.R_Traversal.R_IndexedTraversal' and return a monoidal summary
-- along with the answer.
--
-- @
-- l '<%~' f ≡ l '<%@~' 'const' f
-- @
--
-- When you do not need access to the index then ('<%~') is more liberal in what it can accept.
--
-- If you do not need the intermediate result, you can use ('Control.R_Lens.R_Setter.%@~') or even ('Control.R_Lens.R_Setter.%~').
--
-- @
-- ('<%@~') ::             'R_IndexedLens' i s t a b      -> (i -> a -> b) -> s -> (b, t)
-- ('<%@~') :: 'Monoid' b => 'Control.R_Lens.R_Traversal.R_IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> (b, t)
-- @
(<%@~) :: R_Over (Indexed i) ((,) b) s t a b -> (i -> a -> b) -> s -> (b, t)
l <%@~ f = l (Indexed $ \i a -> let b = f i a in (b, b))
{-# INLINE (<%@~) #-}

-- | Adjust the target of an 'R_IndexedLens' returning the old value, or
-- adjust all of the targets of an 'Control.R_Lens.R_Traversal.R_IndexedTraversal' and return a monoidal summary
-- of the old values along with the answer.
--
-- @
-- ('<<%@~') ::             'R_IndexedLens' i s t a b      -> (i -> a -> b) -> s -> (a, t)
-- ('<<%@~') :: 'Monoid' a => 'Control.R_Lens.R_Traversal.R_IndexedTraversal' i s t a b -> (i -> a -> b) -> s -> (a, t)
-- @
(<<%@~) :: R_Over (Indexed i) ((,) a) s t a b -> (i -> a -> b) -> s -> (a, t)
l <<%@~ f = l $ Indexed $ \i a -> second' (f i) (a,a)

{-# INLINE (<<%@~) #-}

-- | Adjust the target of an 'R_IndexedLens' returning a supplementary result, or
-- adjust all of the targets of an 'Control.R_Lens.R_Traversal.R_IndexedTraversal' and return a monoidal summary
-- of the supplementary results and the answer.
--
-- @
-- ('%%@~') ≡ 'Control.R_Lens.Indexed.withIndex'
-- @
--
-- @
-- ('%%@~') :: 'Functor' f => 'R_IndexedLens' i s t a b      -> (i -> a -> f b) -> s -> f t
-- ('%%@~') :: 'Applicative' f => 'Control.R_Lens.R_Traversal.R_IndexedTraversal' i s t a b -> (i -> a -> f b) -> s -> f t
-- @
--
-- In particular, it is often useful to think of this function as having one of these even more
-- restricted type signatures:
--
-- @
-- ('%%@~') ::             'R_IndexedLens' i s t a b      -> (i -> a -> (r, b)) -> s -> (r, t)
-- ('%%@~') :: 'Monoid' r => 'Control.R_Lens.R_Traversal.R_IndexedTraversal' i s t a b -> (i -> a -> (r, b)) -> s -> (r, t)
-- @
(%%@~) :: R_IndexedLensLike i f s t a b -> (i -> a -> f b) -> s -> f t
(%%@~) l = l .# Indexed
{-# INLINE (%%@~) #-}

-- | Adjust the target of an 'R_IndexedLens' returning a supplementary result, or
-- adjust all of the targets of an 'Control.R_Lens.R_Traversal.R_IndexedTraversal' within the current state, and
-- return a monoidal summary of the supplementary results.
--
-- @
-- l '%%@=' f ≡ 'state' (l '%%@~' f)
-- @
--
-- @
-- ('%%@=') :: 'MonadState' s m                 => 'R_IndexedLens' i s s a b      -> (i -> a -> (r, b)) -> s -> m r
-- ('%%@=') :: ('MonadState' s m, 'Monoid' r) => 'Control.R_Lens.R_Traversal.R_IndexedTraversal' i s s a b -> (i -> a -> (r, b)) -> s -> m r
-- @
(%%@=) :: MonadState s m => R_IndexedLensLike i ((,) r) s s a b -> (i -> a -> (r, b)) -> m r
#if MIN_VERSION_mtl(2,1,0)
l %%@= f = State.state (l %%@~ f)
#else
l %%@= f = do
  (r, s) <- State.gets (l %%@~ f)
  State.put s
  return r
#endif
{-# INLINE (%%@=) #-}

-- | Adjust the target of an 'R_IndexedLens' returning the intermediate result, or
-- adjust all of the targets of an 'Control.R_Lens.R_Traversal.R_IndexedTraversal' within the current state, and
-- return a monoidal summary of the intermediate results.
--
-- @
-- ('<%@=') :: 'MonadState' s m                 => 'R_IndexedLens' i s s a b      -> (i -> a -> b) -> m b
-- ('<%@=') :: ('MonadState' s m, 'Monoid' b) => 'Control.R_Lens.R_Traversal.R_IndexedTraversal' i s s a b -> (i -> a -> b) -> m b
-- @
(<%@=) :: MonadState s m => R_IndexedLensLike i ((,) b) s s a b -> (i -> a -> b) -> m b
l <%@= f = l %%@= \ i a -> let b = f i a in (b, b)
{-# INLINE (<%@=) #-}

-- | Adjust the target of an 'R_IndexedLens' returning the old value, or
-- adjust all of the targets of an 'Control.R_Lens.R_Traversal.R_IndexedTraversal' within the current state, and
-- return a monoidal summary of the old values.
--
-- @
-- ('<<%@=') :: 'MonadState' s m                 => 'R_IndexedLens' i s s a b      -> (i -> a -> b) -> m a
-- ('<<%@=') :: ('MonadState' s m, 'Monoid' b) => 'Control.R_Lens.R_Traversal.R_IndexedTraversal' i s s a b -> (i -> a -> b) -> m a
-- @
(<<%@=) :: MonadState s m => R_IndexedLensLike i ((,) a) s s a b -> (i -> a -> b) -> m a
#if MIN_VERSION_mtl(2,1,0)
l <<%@= f = State.state (l (Indexed $ \ i a -> (a, f i a)))
#else
l <<%@= f = do
  (r, s) <- State.gets (l (Indexed $ \ i a -> (a, f i a)))
  State.put s
  return r
#endif
{-# INLINE (<<%@=) #-}

------------------------------------------------------------------------------
-- R_ALens Combinators
------------------------------------------------------------------------------

-- | A version of ('Control.R_Lens.R_Getter.^.') that works on 'R_ALens'.
--
-- >>> ("hello","world")^#_2
-- "world"
(^#) :: s -> R_ALens s t a b -> a
s ^# l = ipos (l sell s)
{-# INLINE (^#) #-}

-- | A version of 'Control.R_Lens.R_Setter.set' that works on 'R_ALens'.
--
-- >>> storing _2 "world" ("hello","there")
-- ("hello","world")
storing :: R_ALens s t a b -> b -> s -> t
storing l b s = ipeek b (l sell s)
{-# INLINE storing #-}

-- | A version of ('Control.R_Lens.R_Setter..~') that works on 'R_ALens'.
--
-- >>> ("hello","there") & _2 #~ "world"
-- ("hello","world")
( #~ ) :: R_ALens s t a b -> b -> s -> t
( #~ ) l b s = ipeek b (l sell s)
{-# INLINE ( #~ ) #-}

-- | A version of ('Control.R_Lens.R_Setter.%~') that works on 'R_ALens'.
--
-- >>> ("hello","world") & _2 #%~ length
-- ("hello",5)
( #%~ ) :: R_ALens s t a b -> (a -> b) -> s -> t
( #%~ ) l f s = ipeeks f (l sell s)
{-# INLINE ( #%~ ) #-}

-- | A version of ('%%~') that works on 'R_ALens'.
--
-- >>> ("hello","world") & _2 #%%~ \x -> (length x, x ++ "!")
-- (5,("hello","world!"))
( #%%~ ) :: Functor f => R_ALens s t a b -> (a -> f b) -> s -> f t
( #%%~ ) l f s = runPretext (l sell s) f
{-# INLINE ( #%%~ ) #-}

-- | A version of ('Control.R_Lens.R_Setter..=') that works on 'R_ALens'.
( #= ) :: MonadState s m => R_ALens s s a b -> b -> m ()
l #= f = modify (l #~ f)
{-# INLINE ( #= ) #-}

-- | A version of ('Control.R_Lens.R_Setter.%=') that works on 'R_ALens'.
( #%= ) :: MonadState s m => R_ALens s s a b -> (a -> b) -> m ()
l #%= f = modify (l #%~ f)
{-# INLINE ( #%= ) #-}

-- | A version of ('<%~') that works on 'R_ALens'.
--
-- >>> ("hello","world") & _2 <#%~ length
-- (5,("hello",5))
(<#%~) :: R_ALens s t a b -> (a -> b) -> s -> (b, t)
l <#%~ f = \s -> runPretext (l sell s) $ \a -> let b = f a in (b, b)
{-# INLINE (<#%~) #-}

-- | A version of ('<%=') that works on 'R_ALens'.
(<#%=) :: MonadState s m => R_ALens s s a b -> (a -> b) -> m b
l <#%= f = l #%%= \a -> let b = f a in (b, b)
{-# INLINE (<#%=) #-}

-- | A version of ('%%=') that works on 'R_ALens'.
( #%%= ) :: MonadState s m => R_ALens s s a b -> (a -> (r, b)) -> m r
#if MIN_VERSION_mtl(2,1,1)
l #%%= f = State.state $ \s -> runPretext (l sell s) f
#else
l #%%= f = do
  p <- State.gets (l sell)
  let (r, t) = runPretext p f
  State.put t
  return r
#endif
{-# INLINE ( #%%= ) #-}

-- | A version of ('Control.R_Lens.R_Setter.<.~') that works on 'R_ALens'.
--
-- >>> ("hello","there") & _2 <#~ "world"
-- ("world",("hello","world"))
(<#~) :: R_ALens s t a b -> b -> s -> (b, t)
l <#~ b = \s -> (b, storing l b s)
{-# INLINE (<#~) #-}

-- | A version of ('Control.R_Lens.R_Setter.<.=') that works on 'R_ALens'.
(<#=) :: MonadState s m => R_ALens s s a b -> b -> m b
l <#= b = do
  l #= b
  return b
{-# INLINE (<#=) #-}

-- | There is a field for every type in the 'Void'. Very zen.
--
-- >>> [] & mapped.devoid +~ 1
-- []
--
-- >>> Nothing & mapped.devoid %~ abs
-- Nothing
--
-- @
-- 'devoid' :: 'R_Lens'' 'Void' a
-- @
devoid :: R_Over p f Void Void a b
devoid _ = absurd
{-# INLINE devoid #-}

-- | We can always retrieve a @()@ from any type.
--
-- >>> "hello"^.united
-- ()
--
-- >>> "hello" & united .~ ()
-- "hello"
united :: R_Lens' a ()
united f v = f () <&> \ () -> v
{-# INLINE united #-}

-- | Fuse a composition of lenses using 'Yoneda' to provide 'fmap' fusion.
--
-- In general, given a pair of lenses 'foo' and 'bar'
--
-- @
-- fusing (foo.bar) = foo.bar
-- @
--
-- however, @foo@ and @bar@ are either going to 'fmap' internally or they are trivial.
--
-- 'fusing' exploits the 'Yoneda' lemma to merge these separate uses into a single 'fmap'.
--
-- This is particularly effective when the choice of functor 'f' is unknown at compile
-- time or when the 'R_Lens' @foo.bar@ in the above description is recursive or complex
-- enough to prevent inlining.
--
-- @
-- 'fusing' :: 'R_Lens' s t a b -> 'R_Lens' s t a b
-- @
fusing :: Functor f => R_LensLike (Yoneda f) s t a b -> R_LensLike f s t a b
fusing t = \f -> lowerYoneda . t (liftYoneda . f)
{-# INLINE fusing #-}
