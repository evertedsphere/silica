{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Silica
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Usage:
--
-- You can derive lenses automatically for many data types:
--
-- @
-- import Silica
-- 
-- data FooBar a
--   = Foo { _x :: ['Int'], _y :: a }
--   | Bar { _x :: ['Int'] }
-- 'makeLenses' ''FooBar
-- @
--
-- This defines the following lenses:
--
-- @
-- x :: 'Lens'' (FooBar a) ['Int']
-- y :: 'Traversal' (FooBar a) (FooBar b) a b
-- @
--
-- You can then access the value of @_x@ with ('^.'), the value of @_y@ –
-- with ('^?') or ('^?!') (since it can fail), set the values with ('.~'),
-- modify them with ('%~'), and use almost any other combinator that is
-- re-exported here on those fields.
--
-- The combinators here have unusually specific type signatures, so for
-- particularly tricky ones, the simpler type signatures you might want to
-- pretend the combinators have are specified as well.
--
-- More information on how to use lenses is available on the lens wiki:
--
-- <http://github.com/ekmett/lens/wiki>
--
-- <<Hierarchy.png>>
--
-- Optic-creating functions:
--
-- For people used to the original `lens` library:
--
-- @
-- r (raw)         : raw van Laarhoven lens
-- g (general)     : newtyped, most general monomorphic type (use `sub` or a `toFooOptic` function to change)
--                   (provided for consistency; usually this is an indexed or index-preserving optic 
--                   so it will be one of the i- or ip- versions)
-- p (polymorphic) : newtyped, polymorphic (can be instantiated at any supertype)
-- @
--
-- Prefixes for recommended API (all newtyped, monomorphic):
--
-- @
-- [no prefix]    : vanilla/unindexed  - most "sensible" type (no 'Over' or 'Optical')
-- i              : indexed            - using extra @i ->@ argument
-- ip             : index-preserving   - 
-- @
--
-- @
-- gfoo ::               ... -> FooOptic s t a b
-- pfoo :: A_Foo \<: k => ... -> Optic k s t a b
-- rfoo ::               ... -> R_FooOptic s t a b
-- @

----------------------------------------------------------------------------
module Silica
  ( 
    -- module Silica.Cons
  -- , module Silica.At
  -- , module Silica.Each
  -- , module Silica.Empty
  -- , module Silica.Equality
  -- , module Silica.Fold
    module Silica.Getter
  -- , module Silica.Indexed
  -- , module Silica.Iso
  , module Silica.Lens
  -- , module Silica.Level
  -- , module Silica.Plated
  -- , module Silica.Prism
  -- , module Silica.Reified
  -- , module Silica.Review
  , module Silica.Setter
  -- , module Silica.Traversal
  , module Silica.Tuple
  , module Silica.Type
  -- , module Silica.Wrapped
  -- , module Silica.Zoom
  ) where

-- import Silica.At
-- import Silica.Cons
-- import Silica.Each
-- import Silica.Empty
-- import Silica.Equality
-- import Silica.Fold
import Silica.Getter
-- import Silica.Indexed
-- import Silica.Iso
import Silica.Lens
-- import Silica.Level
-- import Silica.Plated
-- import Silica.Prism
-- import Silica.Reified
-- import Silica.Review
import Silica.Setter
-- import Silica.Traversal
import Silica.Tuple
import Silica.Type
-- import Silica.Wrapped
-- import Silica.Zoom

#ifdef HLINT
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
#endif
