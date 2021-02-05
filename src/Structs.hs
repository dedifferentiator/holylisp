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
  , HInt (..)
  , HRead (..)
  , HSub (..)
  , HAdd (..)
  , HVar (..)
  , HLet (..)
  , Atom (..)
  ) where

import Relude hiding (show)
import Text.Show


newtype HInt  = TInt Int          deriving Eq
newtype HVar  = TVar String       deriving Eq
newtype HRead = TRead ()          deriving Eq
newtype HSub  = TSub Exp          deriving Eq
data    HAdd  = TAdd Exp Exp      deriving Eq
data    HLet  = TLet HVar Exp Exp deriving Eq

instance Show HInt  where show (TInt  i) = show i
instance Show HVar  where show (TVar v) = v
instance Show HRead where show (TRead _) = "(read)"
instance Show HSub  where show (TSub  e) = "(- " <> show e <> ")"
instance Show HAdd  where
  show (TAdd e1 e2) = "(+ " <> show e1 <> " " <> show e2 <> ")"
instance Show HLet  where
  show (TLet v expr body) = "(let (["
                            <> show v    <> " "
                            <> show expr <> "]) "
                            <> show body
                            <> ")"

-- | `exp` token
data Exp = HInt HInt   -- ^ `int' token
         | HRead HRead -- ^ `(read)` token
         | HSub HSub   -- ^ `(- exp)` token
         | HAdd HAdd   -- ^ `(+ exp exp)` token
         | HVar HVar   -- ^ `var` token
         | HLet HLet   -- ^ `(let ([var exp]) exp)` token
         deriving Eq

instance Show Exp where
  show (HInt  v) = show v
  show (HRead v) = show v
  show (HSub  v) = show v
  show (HAdd  v) = show v
  show (HVar  v) = show v
  show (HLet  v) = show v

-- | `hlisp` token
data HolyLisp = HolyLisp () Exp deriving Eq

-- | Tokens which don't require optimisations in RMCO pass
data Atom = AInt HInt
          | AVar HVar deriving Eq

instance Show HolyLisp where show (HolyLisp _ e) = show e
instance Show Atom where
  show (AInt v) = show v
  show (AVar v) = show v
