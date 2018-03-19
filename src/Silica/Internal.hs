{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Silica.Internal
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
-- These are some of the explicit 'Functor' instances that leak into the
-- type signatures of @Silica@. You shouldn't need to import this
-- module directly for most use-cases.
--
----------------------------------------------------------------------------
module Silica.Internal
  ( module Silica.Internal.Bazaar
  , module Silica.Internal.Context
  , module Silica.Internal.Fold
  , module Silica.Internal.Getter
  , module Silica.Internal.Indexed
  , module Silica.Internal.Iso
  , module Silica.Internal.Level
  , module Silica.Internal.Magma
  , module Silica.Internal.Prism
  , module Silica.Internal.Review
  , module Silica.Internal.Setter
  , module Silica.Internal.Zoom
  ) where

import Silica.Internal.Bazaar
import Silica.Internal.Context
import Silica.Internal.Fold
import Silica.Internal.Getter
import Silica.Internal.Indexed
import Silica.Internal.Instances ()
import Silica.Internal.Iso
import Silica.Internal.Level
import Silica.Internal.Magma
import Silica.Internal.Prism
import Silica.Internal.Review
import Silica.Internal.Setter
import Silica.Internal.Zoom

#ifdef HLINT
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
#endif
