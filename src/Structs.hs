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
  ) where

import Relude

-- | Represents `exp` token
data Exp = HInt Int     -- ^ `int' terminal
         | HRead ()     -- ^ `(read)` terminal
         | HSub Exp     -- ^ `(- exp)` non-terminal
         | HAdd Exp Exp -- ^ `(+ exp exp)` non-terminal
         deriving (Eq, Show)

-- | Represents `hlisp` token
data HolyLisp = HolyLisp () Exp deriving (Eq, Show)
