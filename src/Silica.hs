{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Silica where

import GHC.Exts (Constraint)
import GHC.TypeLits hiding (type (*))
import Data.Kind
import Data.Bifunctor
import Data.Foldable
import Data.Constraint
import Data.Coerce
import Data.Functor.Contravariant
import Data.Functor.Const
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Tagged
import Data.Profunctor.Closed
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Traversable
import Data.Proxy
import Control.Comonad
import Control.Monad.Fix
import Data.Distributive
import qualified Control.Arrow as Arrow
import Control.Arrow (Kleisli(..))
import Data.Reflection
import Protolude hiding ((.), id, TypeError, Rep, show, to, Text, (%))
import Control.Category
import Data.Functor.Identity

import Data.Profunctor.Unsafe
import Data.Functor.Apply

import Data.Functor.Compose

import Silica.Internal

error :: [Char] -> a
error = panic . strConv Strict

--------------------------------------------------------------------------------
-- basic types
--------------------------------------------------------------------------------

-- TODO remove the `k` parameter since it's always (->)
type Silica 
  (k :: * -> * -> *) 
  (p :: * -> * -> *) 
  (q :: * -> * -> *) 
  (f :: * -> *) 
  (s :: *) 
  (t :: *) 
  (a :: *) 
  (b :: *) 
  = (a `p` f b) `k` (s `q` f t)

type family Cts 
  (o :: *) 
  (k :: * -> * -> *) 
  (p :: * -> * -> *) 
  (q :: * -> * -> *) 
  (f :: * -> *) 
  = (c :: Constraint)

type    Sand o k p q f s t a b     = Cts o k p q f => Silica k p q f s t a b
type    Glass   o      s t a b     = forall p q f k. Sand o k p q f s t a b
newtype Optic   o      s t a b     = Optic { runOptic :: Glass o s t a b }

type    Optic'  o      s  a        = Optic o s s a a
type    Glass'  o      s  a        = Glass o s s a a

type family IxKey (s :: *) :: *
type family IxVal (s :: *) :: *

type instance IxKey [a] = Int
type instance IxVal [a] = a

--------------------------------------------------------------------------------
-- optic tags
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
data A_Getting (r :: *)

data A_Ixed (i :: *) (o :: *)

--------------------------------------------------------------------------------
-- fiddly bits for subtyping and composition
--------------------------------------------------------------------------------

data SubProxy 
  (o :: *) 
  (l :: *) 
  (k :: * -> * -> *) 
  (p :: * -> * -> *) 
  (q :: * -> * -> *) 
  (f :: * -> *) 
  = SubProxy

sub :: forall o l s t a b . (o <: l) => Optic o s t a b -> Optic l s t a b
sub (Optic o) = Optic (implies' o)
  where
    implies' :: forall k p q f. Sand o k p q f s t a b -> Sand l k p q f s t a b
    implies' = implies (SubProxy @o @l @k @p @q @f)

subOut :: (o <: l) => (r -> Optic o s t a b) -> r -> Optic l s t a b
subOut f = sub . f

subIn :: (o <: l) => (Optic l s t a b -> r) -> Optic o s t a b -> r
subIn f = f . sub

-- | Read "can act as" or "is".
class    o <: l where 
  implies :: proxy o l k p q f -> (Cts o k p q f => r) -> (Cts l k p q f => r)

instance o <: o where implies _ r = r

-- commutative
class (o <: m, l <: m) => Join o l m | o l -> m

class Chain o where
  (%%) :: Optic o s t u v -> Optic o u v a b -> Optic o s t a b

(%) :: (Join o l m, Chain m) => Optic o s t u v -> Optic l u v a b -> Optic m s t a b
o % o' = sub o %% sub o'

--------------------------------------------------------------------------------
-- optic types
--------------------------------------------------------------------------------

type Lens                  s t a b = Optic  A_Lens                     s t a b
type Traversal             s t a b = Optic  A_Traversal                s t a b
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
type Setter'               s   a   = Setter                            s s a a
type Prism'                s   a   = Prism                             s s a a
type Iso'                  s   a   = Iso                               s s a a

type IxedLens          i   s t a b = Optic  (A_Ixed i A_Lens)          s t a b
type IxedTraversal     i   s t a b = Optic  (A_Ixed i A_Traversal)     s t a b
type IxedTraversal1    i   s t a b = Optic  (A_Ixed i A_Traversal)     s t a b
type IxedSetter        i   s t a b = Optic  (A_Ixed i A_Setter)        s t a b
type IxedGetter        i   s t a b = Optic  (A_Ixed i A_Getter)        s t a b

type IxedGetting       i r s   a   = Optic' (A_Ixed i (A_Getting r))   s   a
type IxedFold          i   s   a   = Optic' (A_Ixed i A_Fold)          s   a
type IxedFold1         i   s   a   = Optic' (A_Ixed i A_Fold1)         s   a

type IxedLens'         i   s   a   = IxedLens                        i s s a a
type IxedTraversal'    i   s   a   = IxedTraversal                   i s s a a
type IxedTraversal1'   i   s   a   = IxedTraversal1                  i s s a a
type IxedSetter'       i   s   a   = IxedSetter                      i s s a a

-------------------------------------------------------------------------------
-- classes
--------------------------------------------------------------------------------

-- | Anything 'Settable' must be isomorphic to the 'Identity' 'Functor'.
class (Applicative f, Distributive f, Traversable f) => Settable f where
  untainted :: f a -> a

  untaintedDot :: Profunctor p => p a (f b) -> p a b
  untaintedDot g = g `seq` rmap untainted g
  {-# INLINE untaintedDot #-}

  taintedDot :: Profunctor p => p a b -> p a (f b)
  taintedDot g = g `seq` rmap pure g
  {-# INLINE taintedDot #-}

class Ixed m where
  ix :: IxKey m -> Traversal' m (IxVal m) 

class Ixed m => At m where
  at :: IxKey m -> Lens' m (Maybe (IxVal m))

class Index i k where
  index :: ((i -> a) -> b) -> k a b

class Functor f => IxedFunctor i f | f -> i where
  imap :: (i -> a -> b) -> f a -> f b

class Foldable f => IxedFoldable i f | f -> i where
  ifoldMap :: Monoid m => (i -> a -> m) -> f a -> m

class (IxedFunctor i t, IxedFoldable i t, Traversable t) 
  => IxedTraversable i t | t -> i where
  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)

newtype WithIndex i a b = WithIndex { withIndex :: (i -> a) -> b }

--------------------------------------------------------------------------------
-- constraint synonyms for optic-flexibility in input
--------------------------------------------------------------------------------
 
class (o <: A_Lens) => AsLens o
instance (o <: A_Lens) => AsLens o

class (o <: A_Traversal) => AsTraversal o
instance (o <: A_Traversal) => AsTraversal o

class (o <: A_Traversal1) => AsTraversal1 o
instance (o <: A_Traversal1) => AsTraversal1 o

class (o <: A_Prism) => AsPrism o
instance (o <: A_Prism) => AsPrism o

class (o <: A_Iso) => AsIso o
instance (o <: A_Iso) => AsIso o

class (o <: A_Equality) => AsEquality o
instance (o <: A_Equality) => AsEquality o

class (o <: A_Review) => AsReview o
instance (o <: A_Review) => AsReview o

class (o <: A_Fold) => AsFold o
instance (o <: A_Fold) => AsFold o

class (o <: A_Fold1) => AsFold1 o
instance (o <: A_Fold1) => AsFold1 o

class (o <: A_Setter) => AsSetter o
instance (o <: A_Setter) => AsSetter o

class (o <: A_Getter) => AsGetter o
instance (o <: A_Getter) => AsGetter o

class (o <: A_Getting r) => AsGetting r o
instance (o <: A_Getting r) => AsGetting r o

class (o <: A_Getting (Endo r)) => Squashing r o
instance (o <: A_Getting (Endo r)) => Squashing r o

--------------------------------------------------------------------------------
-- building different optic types from plain functions and VL reps
-------------------------------------------------------------------------------- 

----------------------------------------------------------------------
-- Lenses
----------------------------------------------------------------------

-- | Explicitly cast an optic to a lens.
toLens :: AsLens o => Optic o s t a b -> Lens s t a b
toLens = sub
{-# INLINE toLens #-}

-- | Build a lens from the van Laarhoven representation.
vlLens :: LensVL s t a b -> Lens s t a b
vlLens = Optic
{-# INLINE vlLens #-}

-- | Build a lens from a getter and setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = vlLens (\f s -> set s <$> f (get s))
{-# INLINE lens #-}

----------------------------------------------------------------------
-- Traversals
----------------------------------------------------------------------

-- | Explicitly cast an optic to a traversal.
toTraversal :: AsTraversal k => Optic k s t a b -> Traversal s t a b
toTraversal = sub
{-# INLINE toTraversal #-}

-- | Build a traversal from the van Laarhoven representation.
vlTraversal :: TraversalVL s t a b -> Traversal s t a b
vlTraversal = Optic
{-# INLINE vlTraversal #-}

----------------------------------------------------------------------
-- Setters
----------------------------------------------------------------------

-- | Explicitly cast an optic to a setter.
toSetter :: AsSetter k => Optic k s t a b -> Setter s t a b
toSetter = sub
{-# INLINE toSetter #-}

-- | Build a setter from the van Laarhoven representation.
vlSetter :: SetterVL s t a b -> Setter s t a b
vlSetter = Optic
{-# INLINE vlSetter #-}

-- | Build a setter from a function to modify the element(s).
sets :: ((a -> b) -> s -> t) -> Setter s t a b
sets f = Optic (\afb -> taintedDot (f (untainted . afb)))
{-# INLINE sets #-}

----------------------------------------------------------------------
-- Getters and Getting
----------------------------------------------------------------------

-- | Explicitly cast an optic to a getter.
toGetter :: AsGetter k => Optic' k s a -> Getter s a
toGetter = sub
{-# INLINE toGetter #-}

-- | Explicitly cast an optic to a Getting.
toGetting :: AsGetting r k => Optic' k s a -> Getting r s a
toGetting = sub

-- | Build a getter from the van Laarhoven representation.
vlGetter :: GetterVL s a -> Getter s a
vlGetter = Optic
{-# INLINE vlGetter #-}

-- | Build a getter from a function.
to :: (s -> a) -> Getter s a
to f = Optic (dimap f (contramap f))
{-# INLINE to #-}

----------------------------------------------------------------------
-- Folds
----------------------------------------------------------------------
 
-- | Explicitly cast an optic to a fold.
toFold :: AsFold k => Optic' k s a -> Fold s a
toFold = sub
{-# INLINE toFold #-}

-- | Build a fold from the van Laarhoven representation.
vlFold :: FoldVL s a -> Fold s a
vlFold = Optic
{-# INLINE vlFold #-}

----------------------------------------------------------------------
-- Equality
----------------------------------------------------------------------

-- | Explicitly cast an optic to an iso.
toEquality :: AsEquality k => Optic k s t a b -> Equality s t a b
toEquality = sub
{-# INLINE toEquality #-}

-- | Build an equality from the van Laarhoven representation.
vlEquality :: EqualityVL s t a b -> Equality s t a b
vlEquality = Optic
{-# INLINE vlEquality #-}

-- | Proof of reflexivity.
simple :: Equality' a a
simple = Optic id
{-# INLINE simple #-}

-- | Provides a witness of equality.
data Identical (s :: *) (t :: *) (a :: *) (b :: *) where
  Identical :: (s ~ a, t ~ b) => Identical s t a b

-- | Obtain a witness for an equality.
runEq :: AsEquality k => Optic k s t a b -> Identical s t a b
runEq o =
  case runOptic (toEquality o) Identical of
    Identical -> Identical
{-# INLINE runEq #-}

----------------------------------------------------------------------
-- Reviews
----------------------------------------------------------------------

-- | Explicitly cast an optic to a review.
toReview :: AsReview k => Optic' k t b -> Review t b
toReview = sub
{-# INLINE toReview #-}

-- | Build a review from the van Laarhoven representation.
vlReview :: ReviewVL t b -> Review t b
vlReview = Optic
{-# INLINE vlReview #-}

-- | A review can be used as a getter from the small to the big type.
re :: AsReview k => Optic' k s a -> Getter a s
re o = to (runIdentity . unTagged . runOptic (toReview o) . Tagged . Identity)

----------------------------------------------------------------------
-- Prisms
----------------------------------------------------------------------

-- | Explicitly cast an optic to a prism.
toPrism :: AsPrism k => Optic k s t a b -> Prism s t a b
toPrism = sub
{-# INLINE toPrism #-}

-- | Build a prism from the van Laarhoven representation.
vlPrism :: PrismVL s t a b -> Prism s t a b
vlPrism = Optic
{-# INLINE vlPrism #-}

-- | Build a prism from a constructor and a matcher.
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism construct match = vlPrism (dimap match (either pure (fmap construct)) . right')
{-# INLINE prism #-}

----------------------------------------------------------------------
-- Iso
----------------------------------------------------------------------

-- | Explicitly cast an optic to an iso.
toIso :: AsIso k => Optic k s t a b -> Iso s t a b
toIso = sub
{-# INLINE toIso #-}

-- | Build an iso from the van Laarhoven representation.
vlIso :: IsoVL s t a b -> Iso s t a b
vlIso = Optic
{-# INLINE vlIso #-}

-- | Build an iso from a pair of inverse functions.
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso f g = Optic (dimap f (fmap g))
{-# INLINE iso #-}

-- | Extract the two components of an isomorphism.
withIso :: AsIso k => Optic k s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso o k = case runOptic (toIso o) (Exchange id Identity) of
  Exchange sa bt -> k sa (runIdentity . bt)
{-# INLINE withIso #-}

-- | Invert an isomorphism.
from :: AsIso k => Optic k s t a b -> Iso b a t s
from o = withIso o $ \ sa bt -> iso bt sa
{-# INLINE from #-}

--------------------------------------------------------------------------------
-- concrete optics and optic classes/families
-------------------------------------------------------------------------------- 

-- | Traversal via the 'Traversal' class.
traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = vlTraversal traverse
{-# INLINE traversed #-}

ignored :: Traversal s s a b
ignored = Optic (const pure)

both :: Traversal (a, a) (b, b) a b
both = vlTraversal (\f (a, a') -> (,) <$> f a <*> f a')

-- | Build a setter from a functor.
mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap

_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ \case
   Left a -> Right a
   Right a -> Left (Right a)

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ \case
   Right a -> Right a
   Left a -> Left (Left a)

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ \case
  Just a -> Right a
  Nothing -> Left Nothing

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism (const Nothing) $ \case
  Nothing -> Right ()
  x -> Left x

class Field1 s t a b where
  _1 :: Lens s t a b

class Field2 s t a b where
  _2 :: Lens s t a b

instance (a' ~ a, b' ~ b) => Field1 (a, x) (b, x) a' b' where
  _1 = lens fst (\(_,x) b -> (b,x))

instance (a' ~ a, b' ~ b) => Field2 (x, a) (x, b) a' b' where
  _2 = lens snd (\(x,_) b -> (x,b))

type family NatOrdinal n where
  NatOrdinal 1 = "first"
  NatOrdinal 2 = "second"

type family ErrListField n a where
  ErrListField n a 
    = Text "You tried to access the " :<>: Text (NatOrdinal n) :<>: Text " field of a list."
    :$$: Text "However, a list does not have any \"fields\". Tuples and similar types can."
    :$$: Text ""
    :$$: ErrListFieldTuple a
    :$$: Text ""

type family ErrListFieldTuple a where
  ErrListFieldTuple (x, y) = Text "You have a list of tuples of type " :<>: ShowType (x,y) :<>: Text "."
    :$$: Text "Try applying `folded` or a similar combinator to first traverse \"into\" the list."
    :$$: Text "Then you can use field selector lenses like _1 to access the fields of the tuples inside."
    :$$: Text ""
    :$$: Text "For example,"
    :$$: Text ">>> [(1,1),(2,4),(3,7)] & sumOf (folded % _2)"
    :$$: Text "12"
    :$$: Text ""
    :$$: Text ">>> [(1,1),(2,4),(3,7)] & sumOf _2"
    :$$: Text "<this error>"
    :$$: Text ""
    :$$: Text "Use `folded` as many times as you need to to drill down into nested structures."
    :$$: Text "For example, here's a nested list:"
    :$$: Text ">>> [[(1,1),(2,4),(3,7)],[(5,6)],[(2,1),(4,3)]] & sumOf (folded % folded % _2)"
    :$$: Text "22"
  ErrListFieldTuple _ = Text ""

instance TypeError (ErrListField 1 a) => Field1 [a] [b] c d where
  _1 = error "field1"

instance TypeError (ErrListField 2 a) => Field2 [a] [b] c d where
  _2 = error "field2"

combine :: (Num a, AsGetting a k, AsGetting a l) => (a -> a -> a) -> Optic' k s a -> Optic' l s a -> Getter s a
combine f g1 g2 = to (\s -> f (s ^. g1) (s ^. g2))

(+.) :: (Num a, AsGetting a k, AsGetting a l) => Optic' k s a -> Optic' l s a -> Getter s a
(+.) = combine (+)

(*.) :: (Num a, AsGetting a k, AsGetting a l) => Optic' k s a -> Optic' l s a -> Getter s a
(*.) = combine (*)

infixl 6 +.
infixl 7 *.

instance Num a => Num (Getting a s a) where
  fromInteger n = sub (to (const (fromInteger n)))
  g + h = sub (combine (+) g h)
  g * h = sub (combine (*) g h)

--------------------------------------------------------------------------------
-- combinators
-------------------------------------------------------------------------------- 

-- | Apply a getter.
view :: AsGetting a k => Optic' k s a -> s -> a
view o = getConst . runOptic (toGetting o) Const
{-# INLINE view #-}

-- | Apply a review.
review :: AsReview k => Optic' k s a -> a -> s
review = view . re

-- | Apply a setter as a modifier.
over :: AsSetter k => Optic k s t a b -> (a -> b) -> s -> t
over o ab = runIdentity #. runOptic (toSetter o) (Identity #. ab)
{-# INLINE over #-}

-- | Apply a setter.
set :: AsSetter k => Optic k s t a b -> b -> s -> t
set o b = runIdentity #. runOptic (toSetter o) (\_ -> Identity b)
{-# INLINE set #-}

-- | Fold via embedding into a monoid.
foldMapOf :: (Monoid r, AsGetting r k) => Optic' k s a -> (a -> r) -> s -> r
foldMapOf o ar = getConst #. runOptic (toGetting o) (Const #. ar)
{-# INLINE foldMapOf #-}

-- | Fold right-associatively.
foldrOf :: Squashing r k => Optic' k s a -> (a -> r -> r) -> r -> s -> r
foldrOf o arr r = flip appEndo r . foldMapOf o (Endo #. arr)
{-# INLINE foldrOf #-}

-- | Fold left-associatively, and strictly.
foldlOf' :: Squashing (r -> r) k => Optic' k s a -> (r -> a -> r) -> r -> s -> r
foldlOf' o rar r0 s = foldrOf o (\a rr r -> rr $! rar r a) id s r0
{-# INLINE foldlOf' #-}

-- | Fold to a list.
toListOf :: Squashing [a] k => Optic' k s a -> s -> [a]
toListOf o = foldrOf o (:) []
{-# INLINE toListOf #-}

-- | Fold to the first element (if it exists).
preview :: (MonadReader s m, AsGetting (First a) k) => Optic' k s a -> m (Maybe a)
preview o = asks (getFirst #. foldMapOf o (First #. Just))
{-# INLINE preview #-}

andOf :: AsGetting All k => Optic' k s Bool -> s -> Bool
andOf o = getAll . foldMapOf o All
{-# INLINE andOf #-}

orOf :: AsGetting Any k => Optic' k s Bool -> s -> Bool
orOf o = getAny . foldMapOf o Any
{-# INLINE orOf #-}

allOf :: AsGetting All k => Optic' k s a -> (a -> Bool) -> s -> Bool
allOf o f = getAll . foldMapOf o (All . f)
{-# INLINE allOf #-}

anyOf :: AsGetting Any k => Optic' k s a -> (a -> Bool) -> s -> Bool
anyOf o f = getAny . foldMapOf o (Any . f)
{-# INLINE anyOf #-}

noneOf :: AsGetting Any k => Optic' k s a -> (a -> Bool) -> s -> Bool
noneOf o f = not . anyOf o f
{-# INLINE noneOf #-}

productOf :: (Squashing (a -> a) k, Num a) => Optic' k s a -> s -> a
productOf o = foldlOf' o (*) 1
{-# INLINE productOf #-}

sumOf :: (Squashing (a -> a) k, Num a) => Optic' k s a -> s -> a
sumOf o = foldlOf' o (+) 0
{-# INLINE sumOf #-}

asumOf :: (Squashing (f a) k, Alternative f) => Optic' k s (f a) -> s -> f a
asumOf o = foldrOf o (<|>) empty
{-# INLINE asumOf #-}

msumOf :: (Squashing (m a) k, MonadPlus m) => Optic' k s (m a) -> s -> m a
msumOf o = foldrOf o mplus mzero
{-# INLINE msumOf #-}

elemOf :: (AsGetting Any k, Eq a) => Optic' k s a -> a -> s -> Bool
elemOf o = anyOf o . (==)
{-# INLINE elemOf #-}

notElemOf :: (AsGetting All k, Eq a) => Optic' k s a -> a -> s -> Bool
notElemOf o = allOf o . (/=)
{-# INLINE notElemOf #-}

concatOf :: AsGetting [a] k => Optic' k s [a] -> s -> [a]
concatOf o = foldMapOf o id
{-# INLINE concatOf #-}

concatMapOf :: AsGetting [b] k => Optic' k s a -> (a -> [b]) -> s -> [b]
concatMapOf = foldMapOf
{-# INLINE concatMapOf #-}

lengthOf :: Squashing (Int -> Int) k => Optic' k s a -> s -> Int
lengthOf o = foldlOf' o (\n _ -> 1 + n) 0
{-# INLINE lengthOf #-}

glengthOf :: (Num i, Squashing (i -> i) k) => Optic' k s a -> s -> i
glengthOf o = foldlOf' o (\n _ -> 1 + n) 0
{-# INLINE glengthOf #-}

-- | Obtain a 'Fold' by lifting an operation that returns a 'Foldable' result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into a 'Fold'.
--
-- >>> [1,2,3,4]^..folding tail
-- [2,3,4]
folding :: Foldable f => (s -> f a) -> Fold s a
folding sfa = Optic (\agb -> phantom . traverse_ agb . sfa)
{-# INLINE folding #-}

ifolding :: Foldable f => (s -> f (i, a)) -> Fold s a
ifolding sfa = Optic (\f -> phantom . traverse_ (phantom . uncurry (indexed f)) . sfa)
{-# INLINE ifolding #-}

noEffect :: (Contravariant f, Applicative f) => f a
noEffect = phantom (pure ())

-- | Obtain a 'Fold' by lifting 'foldr' like function.
--
-- >>> [1,2,3,4]^..foldring foldr
-- [1,2,3,4]

foldring :: (forall f. (Contravariant f, Applicative f) => (a -> f a -> f a) -> f a -> s -> f a) -> Fold s a
foldring fr = Optic (\f -> phantom . fr (\a fa -> f a *> fa) noEffect)
{-# INLINE foldring #-}

-- | Obtain 'FoldWithIndex' by lifting 'ifoldr' like function.
ifoldring 
  :: (forall p f. (Contravariant f, Applicative f) => (i -> a -> f a -> f a) -> f a -> s -> f a) 
  -> Optic (A_Ixed i A_Fold) s s a a
ifoldring ifr = Optic (\f -> phantom . ifr (\i a fa -> indexed f i a *> fa) noEffect)
{-# INLINE ifoldring #-}

-- | Obtain a 'Fold' from any 'Foldable' indexed by ordinal position.
--
-- >>> Just 3^..folded
-- [3]
--
-- >>> Nothing^..folded
-- []
--
-- >>> [(1,2),(3,4)]^..folded.both
-- [1,2,3,4]

folded :: Foldable f => IxedFold Int (f a) a
folded = Optic (conjoined (runOptic (foldring foldr)) (runOptic (ifoldring ifoldr)))
{-# INLINE folded #-}

ifoldr :: Foldable f => (Int -> a -> b -> b) -> b -> f a -> b
ifoldr f z xs = foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldr #-}

----------------------------------------------------------------------
-- infix forms of common operators
----------------------------------------------------------------------

(#) :: AsReview k => Optic' k t b -> b -> t
(#) = review

-- | Flipped infix version of 'view'.
(^.) :: AsGetting a k => s -> Optic' k s a -> a
(^.) = flip view
{-# INLINE (^.) #-}

infixl 8 ^.

-- | Flipped infix version of 'toListOf'.
(^..) :: AsGetting (Endo [a]) k => s -> Optic' k s a -> [a]
(^..) = flip toListOf
{-# INLINE (^..) #-}

infixl 8 ^..

-- | Flipped infix version of 'preview'.
(^?) :: AsGetting (First a) k => s -> Optic' k s a -> Maybe a
(^?) = flip preview
{-# INLINE (^?) #-}

infixl 8 ^?

-- | Infix version of 'over'.
(%~) :: AsSetter k => Optic k s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

infixr 4 %~

-- | Infix version of 'set'.
(.~) :: AsSetter k => Optic k s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

infixr 4 .~

--------------------------------------------------------------------------------
--
-- reified composition and casting rules
-- (the boilerplatey stuff begins here)
--
--------------------------------------------------------------------------------

data Bogus
class TypeError (Text "Absurd!") => Absurd where

type Fn k = k ~ (->)
type Fn3 k p q = (k ~ (->), p ~ (->), q ~ (->))

--------------------------------------------------------------------------------
-- optic constraints
--------------------------------------------------------------------------------

type instance Cts A_Equality    k p q f = (Fn k, p ~ q)
type instance Cts A_Iso         k p q f = (Fn k, p ~ q, Profunctor p, Functor f)
type instance Cts A_Review      k p q f = (Fn k, p ~ q, Choice p, Bifunctor p, Settable f)
type instance Cts A_Prism       k p q f = (Fn k, p ~ q, Choice p, Applicative f)

type instance Cts A_Setter      k p q f = (Fn3 k p q, Settable f)

type instance Cts A_Lens        k p q f = (Fn3 k p q, Functor f)
type instance Cts A_Getter      k p q f = (Fn3 k p q, Contravariant f, Functor f)

type instance Cts A_Traversal   k p q f = (Fn3 k p q, Applicative f)
type instance Cts A_Traversal1  k p q f = (Fn3 k p q, Apply f)

type instance Cts A_Fold        k p q f = (Fn3 k p q, Contravariant f, Applicative f)
type instance Cts A_Fold1       k p q f = (Fn3 k p q, Contravariant f, Apply f)

type instance Cts (A_Getting r) k p q f = (Fn3 k p q, f ~ Const r)

type instance Cts Bogus         k p q f = Absurd

type instance Cts (A_Ixed i A_Lens)      k p q f = (Fn k, Fn q, Ixable i p, Functor f)
type instance Cts (A_Ixed i A_Traversal) k p q f = (Fn k, Fn q, Ixable i p, Applicative f)
type instance Cts (A_Ixed i A_Setter)    k p q f = (Fn k, Fn q, Ixable i p, Settable f)
type instance Cts (A_Ixed i (A_Getting r)) k p q f = (Fn k, Fn q, p ~ Indexed_ i, f ~ Const r)
type instance Cts (A_Ixed i A_Fold)    k p q f = (Fn k, Fn q, Ixable i p, Contravariant f, Applicative f)

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

instance Chain (A_Ixed i A_Fold) where l %% r = Optic (runOptic l . runOptic r)

--------------------------------------------------------------------------------
-- the subtyping lattice
--------------------------------------------------------------------------------

instance k           <: Bogus where implies = error "bogus"

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

instance NotSubtypeError A_Getter A_Setter => A_Getter <: A_Setter where implies _ = error "subtype"

instance NotSubtypeError A_Traversal A_Getter => A_Traversal <: A_Getter where implies _ = error "subtype"

instance NotSubtypeError (A_Getting r) A_Setter => (A_Getting r) <: A_Setter where implies _ = error "subtype"

instance NotSubtypeError (A_Getting r) A_Getter => (A_Getting r) <: A_Getter where implies _ = error "subtype"

instance A_Ixed i A_Lens <: A_Lens where implies _ r = r
instance A_Ixed i A_Fold <: A_Fold where implies _ r = r

instance Monoid r => A_Ixed i A_Fold <: A_Getting r where implies _ r = r

--------------------------------------------------------------------------------
-- Join rules
--------------------------------------------------------------------------------

instance Join o           o           o

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

instance Join (A_Ixed i A_Fold) A_Traversal A_Fold
instance Join (A_Ixed i A_Fold) A_Lens A_Fold

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
       ( Text "The function used "
    :<>: Text "requires a "
    :<>: ShowOptic r
    :<>: Text " argument, "
    :$$: Text "The optic supplied was only a "
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
  NotSubtypeExplanation l r = Text "NotSubtypeExplanation: internal error: " :$$: ShowType l :$$: ShowType r

--------------------------------------------------------------------------------
-- Van Laarhoven types for interop
--------------------------------------------------------------------------------

type EqualityVL s t a b   = forall (p :: * -> * -> *) (f :: * -> *). p a (f b) -> p s (f t)
type IsoVL s t a b        = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type PrismVL s t a b      = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
type LensVL s t a b       = forall f. Functor f => (a -> f b) -> s -> f t
type TraversalVL s t a b  = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal1VL s t a b = forall f. Apply f => (a -> f b) -> s -> f t
type SetterVL s t a b     = forall f. Settable f => (a -> f b) -> s -> f t
type GetterVL s a         = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s
type FoldVL s a           = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s
type Fold1VL s a          = forall f. (Contravariant f, Apply f) => (a -> f a) -> s -> f s
type ReviewVL t b         = forall p f. (Choice p, Bifunctor p, Settable f) => p b (f b) -> p t (f t)
type GettingVL r s a      = (a -> Const r a) -> s -> Const r s 

--------------------------------------------------------------------------------
-- instances
--------------------------------------------------------------------------------

-- | So you can pass our 'Control.Lens.Setter.Setter' into combinators from other lens libraries.
instance Settable Identity where
  untainted = runIdentity
  {-# INLINE untainted #-}
  untaintedDot = (runIdentity #.)
  {-# INLINE untaintedDot #-}
  taintedDot = (Identity #.)
  {-# INLINE taintedDot #-}

instance (Settable f, Settable g) => Settable (Compose f g) where
  untainted = untaintedDot (untaintedDot getCompose)
  {-# INLINE untainted #-}

instance Index i (->) where
  index :: ((i -> a) -> b) -> a -> b
  index f = f . const

instance (i ~ j) => Index i (WithIndex j) where
  index :: ((i -> a) -> b) -> WithIndex j a b
  index = WithIndex

--------------------------------------------------------------------------------
-- helper types
--------------------------------------------------------------------------------
  
-- | Type to represent the components of an isomorphism.
data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Functor (Exchange a b s) where
  fmap tt (Exchange sa bt) = Exchange sa (tt . bt)
  {-# INLINE fmap #-}

instance Profunctor (Exchange a b) where
  dimap ss tt (Exchange sa bt) = Exchange (sa . ss) (tt . bt)
  {-# INLINE dimap #-}
