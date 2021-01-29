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

module Eval
  ( runO1
  , runProg
  ) where

import Parser
import Relude
import Structs
import Data.Text (unpack)

-- | Runs optimisation on AST
runO1 :: HolyLisp -> HolyLisp
runO1 (HolyLisp m e) = HolyLisp m g & if f == g then id else (& runO1)
  where
    f = optimise e
    g = optimise f

-- | Performs trivial optimisations on `Exp`
optimise :: Exp -> Exp
optimise exp =
  case exp of
    -- TODO: add optimisations for such exps as
    -- `(+ int (+ exp int) = (+ exp (+ int + int))`, etc

    HAdd (HInt i1) (HInt i2)        -> HInt $ i1 + i2
    HAdd (HInt i1) (HSub (HInt i2)) -> HInt $ i1 - i2
    HAdd (HSub (HInt i1)) (HInt i2) -> HInt $ i2 - i1

    HAdd (HRead m) e -> HAdd (HRead m) $ optimise e
    HAdd e (HRead m) -> HAdd (optimise e) $ HRead m

    HAdd (HInt i1) e -> HAdd (HInt i1) $ optimise e
    HAdd e (HInt i2) -> HAdd (optimise e) $ HInt i2
    HAdd e1 e2       -> HAdd (optimise e1) $ optimise e2


    HSub (HInt i)  -> HInt $ -i
    HSub (HRead m) -> HSub $ HRead m
    HSub (HSub e)  -> e
    HSub (HVar s)  -> HSub $ HVar s
    HSub exp       -> HSub $ optimise exp

    HLet v e1 e2 -> HLet v (optimise e1) $ optimise e2

    _ -> exp

-- | Replaces all `hvar` occurrences in `body` with `exp`, preserving shadowing
resolveVar :: HVar -> Exp -> Exp -> Exp
resolveVar hvar exp body =
  case body of
    HLet v e1 e2 -> resolveVar hvar exp $ resolveVar v e1 e2

    HAdd (HInt i1) (HInt i2) -> HInt $ i1 + i2
    HAdd (HVar v) e -> if v == hvar then HAdd exp e else HAdd (HVar v) e
    HAdd e (HVar v) -> if v == hvar then HAdd e exp else HAdd e (HVar v)

    HSub (HVar v) -> if v == hvar then HSub exp else HSub (HVar v)

    _ -> exp


-- | Interprets `Exp`
interpretExp :: Exp -> IO Exp
interpretExp exp = do
  case exp of
    HInt i -> return $ HInt i

    HRead _ -> HInt . fromMaybe (error "err: cannot parse Int") . readMaybe @Int . unpack <$> getLine

    HSub (HInt i) -> return $ HInt $ -i
    HSub e -> interpretExp . HSub =<< interpretExp e

    HAdd (HInt i1) (HInt i2) -> return $ HInt $ i1 + i2
    HAdd i1@(HInt _) e -> interpretExp .      HAdd i1 =<< interpretExp e
    HAdd e i2@(HInt _) -> interpretExp . flip HAdd i2 =<< interpretExp e
    HAdd e1 e2         -> interpretExp =<< HAdd <$> interpretExp e1 <*> interpretExp e2

    HVar v -> return $ HVar v

    HLet v e body -> interpretExp $ resolveVar v e body


-- | Interprtes hlisp into the smallest possible ast
runProg :: HolyLisp -> IO HolyLisp
runProg (HolyLisp m e) = HolyLisp m <$> interpretExp e
