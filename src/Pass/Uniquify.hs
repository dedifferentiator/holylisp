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

module Pass.Uniquify where

import Pass.State
import Relude
import Structs

-- | Makes all names of variables unique
uniquify :: Uniq m => HolyLisp -> m HolyLisp
uniquify (HolyLisp m ex) = HolyLisp m <$> go ex
  where
    go :: Uniq m => Exp -> m Exp
    go expr =
      case expr of
        HLet (TLet v e1 e2) -> do
          nv <- genVar v
          HLet . TLet nv e1 . replaceVar v nv <$> go e2

        HAdd (TAdd e1 e2)   -> HAdd <$> (TAdd <$> go e1 <*> go e2)
        HSub (TSub e)       -> HSub . TSub <$> go e

        _ -> return expr

-- | Replaces `from` with `to` in `exp`
replaceVar :: HVar -> HVar -> Exp -> Exp
replaceVar from to expr =
  case expr of
    HVar v -> HVar $ if v == from then to else v

    HLet (TLet v e1 e2)    -> HLet . TLet v (rpl e1) $ rpl e2

    HAdd (TAdd (HVar v) e) -> rpl e & (HAdd .      TAdd (HVar $ if v == from then to else v))
    HAdd (TAdd e (HVar v)) -> rpl e & (HAdd . flip TAdd (HVar $ if v == from then to else v))

    HSub (TSub (HVar v))   -> HSub . TSub . HVar $ if from == v then to else v
    HSub (TSub e)          -> rpl e

    _ -> expr
  where rpl = replaceVar from to
