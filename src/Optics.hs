{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Optics
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
-- import Optics
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
----------------------------------------------------------------------------
module Optics
  ( 
    -- module Optics.Cons
  -- , module Optics.At
  -- , module Optics.Each
  -- , module Optics.Empty
  -- , module Optics.Equality
  -- , module Optics.Fold
  -- , module Optics.Getter
  -- , module Optics.Indexed
  -- , module Optics.Iso
    module Optics.Lens
  -- , module Optics.Level
  -- , module Optics.Plated
  -- , module Optics.Prism
  -- , module Optics.Reified
  -- , module Optics.Review
  -- , module Optics.Setter
  -- , module Optics.Traversal
  -- , module Optics.Tuple
  , module Optics.Type
  -- , module Optics.Wrapped
  -- , module Optics.Zoom
  ) where

-- import Optics.At
-- import Optics.Cons
-- import Optics.Each
-- import Optics.Empty
-- import Optics.Equality
-- import Optics.Fold
-- import Optics.Getter
-- import Optics.Indexed
-- import Optics.Iso
import Optics.Lens
-- import Optics.Level
-- import Optics.Plated
-- import Optics.Prism
-- import Optics.Reified
-- import Optics.Review
-- import Optics.Setter
-- import Optics.Traversal
-- import Optics.Tuple
import Optics.Type
-- import Optics.Wrapped
-- import Optics.Zoom

#ifdef HLINT
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
#endif
