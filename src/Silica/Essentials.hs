{-# LANGUAGE CPP #-}

-- | The most common types and functions in the package.
--
-- For many of the things below, there are more specialised types
-- in the comments. If it helps, you can think of the function or 
-- optic as having one of those types. For example, the function
-- 'lens' has type 
--
-- @
-- (s -> a) -> (s -> b -> t) -> 'Lens' s t a b
-- @
--
-- but you can also think of it as having the following
-- "less polymorphic" or more restricted type (the two types below
-- are the same, and 'Lens'' is a convenient synonym):
--
-- @
-- (s -> a) -> (s -> a -> s) -> 'Lens'  s s a a
-- (s -> a) -> (s -> a -> s) -> 'Lens'' s   a
-- @
--
-- We strive to expose as few infix operators as possible from here,
-- restricting ourselves to a budget of 5 or 6 ('^.', '.~', '%~', '^..', and '^?' 
-- for now) that are so ubiquitous that beginners will run into them very soon, whether
-- they want to or not :)
--
-- Here are some examples that use optics and functions defined in this module. Hopefully one of
-- these resembles some part of something you need to do, in which case you can click on
-- the combinators used to learn more about them.  /basically a list of "idioms" like in J-land/
--
-- = Working with Maybe, Either, tuples, and so on
--
-- @
-- (3,4) '&' 'over' '_2' (* 3) == (9, 4)
-- (3,4) '&' '_2' '%~' (* 3)  == (9, 4)
-- @
--
-- @
-- 'Left' 2 '&' 'set' '_Left' 9 == 'Left' 9
-- 'Left' 2 '&' set '_Right' 9 == 'Left' 2
-- 'Left' 2 '&' '_Right' '.~' 9 == 'Left' 2
-- @

module Silica.Essentials 
  (

  -- * Make optics from simple functions
    lens
  , sets

  -- * Set the values of things focused on by an optic  
  -- | There are infix versions of these functions further down.
  , set, over

  -- * Traversing and transforming data structures
  , mapped

  -- * (infix) set the values of things focused on by an optic
  , (.~), (%~)
  ) where

import Silica.Lens
import Silica.Setter
import Silica.Type
