{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Optics.Internal
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
-- These are some of the explicit 'Functor' instances that leak into the
-- type signatures of @Optics@. You shouldn't need to import this
-- module directly for most use-cases.
--
----------------------------------------------------------------------------
module Optics.Internal
  ( module Optics.Internal.Bazaar
  , module Optics.Internal.Context
  , module Optics.Internal.Fold
  , module Optics.Internal.Getter
  , module Optics.Internal.Indexed
  , module Optics.Internal.Iso
  , module Optics.Internal.Level
  , module Optics.Internal.Magma
  , module Optics.Internal.Prism
  , module Optics.Internal.Review
  , module Optics.Internal.Setter
  , module Optics.Internal.Zoom
  ) where

import Optics.Internal.Bazaar
import Optics.Internal.Context
import Optics.Internal.Fold
import Optics.Internal.Getter
import Optics.Internal.Indexed
import Optics.Internal.Instances ()
import Optics.Internal.Iso
import Optics.Internal.Level
import Optics.Internal.Magma
import Optics.Internal.Prism
import Optics.Internal.Review
import Optics.Internal.Setter
import Optics.Internal.Zoom

#ifdef HLINT
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
#endif
