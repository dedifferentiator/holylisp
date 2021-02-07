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

import Relude hiding (exp)
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
    HAdd e -> optAdd e

    HSub (TSub (HInt (TInt i)))   -> HInt . TInt $ -i
    HSub (TSub (HRead (TRead m))) -> HSub . TSub $ HRead $ TRead m
    HSub (TSub (HSub (TSub e)))   -> e
    HSub (TSub (HVar (TVar s)))  -> HSub $ TSub $ HVar $ TVar s
    HSub (TSub e)        -> HSub . TSub $ optimise e

    HLet (TLet v e1 e2) -> HLet $ TLet v (optimise e1) $ optimise e2

    _ -> exp

optAdd :: HAdd -> Exp
optAdd expr@TAdd{} =
  case expr of
    TAdd (HInt (TInt i1)) (HInt (TInt i2))               -> HInt . TInt $ i1 + i2
    TAdd (HInt (TInt i1)) (HSub (TSub (HInt (TInt i2)))) -> HInt . TInt $ i1 - i2
    TAdd (HSub (TSub (HInt (TInt i1)))) (HInt (TInt i2)) -> HInt . TInt $ i2 - i1

    TAdd (HRead (TRead m)) e -> HAdd $ TAdd (HRead $ TRead m) $ optimise e
    TAdd e (HRead (TRead m)) -> HAdd $ TAdd (optimise e) $ HRead $ TRead m

    TAdd (HInt i1) e        -> HAdd $ TAdd (HInt i1) $ optimise e
    TAdd e (HInt (TInt i2)) -> HAdd $ TAdd (optimise e) $ HInt $ TInt i2
    TAdd e1 e2              -> HAdd $ TAdd (optimise e1) $ optimise e2

-- | Replaces all `hvar` occurrences with `exp` in `body`, preserving shadowing
resolveVar :: HVar -> Exp -> Exp -> Exp
resolveVar hvar exp body =
  case body of
    HLet (TLet v e1 e2) -> resolveVar hvar exp $ resolveVar v e1 e2

    HAdd (TAdd (HInt (TInt i1)) (HInt (TInt i2))) -> HInt . TInt $ i1 + i2
    HAdd (TAdd (HVar v) e) -> if v == hvar then HAdd $ TAdd exp e else HAdd $ TAdd (HVar v) e
    HAdd (TAdd e (HVar v)) -> if v == hvar then HAdd $ TAdd e exp else HAdd $ TAdd e (HVar v)

    HSub (TSub (HVar v)) -> if v == hvar then HSub $ TSub exp else HSub $ TSub (HVar v)

    _ -> exp

-- | Interprets `Exp`
interpretExp :: Exp -> IO Exp
interpretExp exp = do
  case exp of
    HInt i -> return $ HInt i

    HRead (TRead _) -> HInt . TInt . fromMaybe (error "err: cannot parse Int") . readMaybe @Int . unpack <$> getLine

    HSub (TSub (HInt (TInt i))) -> return . HInt . TInt $ -i
    HSub (TSub e) -> interpretExp . HSub . TSub =<< interpretExp e

    HAdd (TAdd (HInt (TInt i1)) (HInt (TInt i2))) -> return . HInt . TInt $ i1 + i2
    HAdd (TAdd i1@(HInt _) e) -> interpretExp . HAdd .      TAdd i1 =<< interpretExp e
    HAdd (TAdd e i2@(HInt _)) -> interpretExp . HAdd . flip TAdd i2 =<< interpretExp e
    HAdd (TAdd e1 e2)        -> interpretExp . HAdd =<< TAdd <$> interpretExp e1 <*> interpretExp e2

    HVar v -> return $ HVar v

    HLet (TLet v e body) -> interpretExp $ resolveVar v e body

-- | Interprtes hlisp into the smallest possible ast
runProg :: HolyLisp -> IO HolyLisp
runProg (HolyLisp m e) = HolyLisp m <$> interpretExp e
