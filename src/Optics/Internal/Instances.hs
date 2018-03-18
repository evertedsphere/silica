-----------------------------------------------------------------------------
-- |
-- Module      :  Optics.Internal.Instances
-- Copyright   :  (C) 2012-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module includes orphan instances for @(,)@, 'Either' and 'Const' that
-- should be supplied by base. These have moved to @semigroupoids@ as of 4.2.
----------------------------------------------------------------------------
module Optics.Internal.Instances () where

import Data.Orphans ()
import Data.Traversable.Instances ()
