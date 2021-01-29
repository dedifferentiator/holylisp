-- Copyright (C) 2021 dedifferentiator

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

module Structs
  ( Exp (..)
  , HolyLisp (..)
  , HVar (..)
  ) where

import Relude

-- | Represents `var` token
newtype HVar = TVar String deriving (Eq, Show)

-- | Represents `exp` token
data Exp = HInt Int     -- ^ `int' token
         | HRead ()     -- ^ `(read)` token
         | HSub Exp     -- ^ `(- exp)` token
         | HAdd Exp Exp -- ^ `(+ exp exp)` token
         | HVar HVar    -- ^ `var` token
         | HLet HVar Exp Exp -- ^ `(let ([var exp]) exp) token
         deriving (Eq, Show)

-- | Represents `hlisp` token
data HolyLisp = HolyLisp () Exp deriving (Eq, Show)
