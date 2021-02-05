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

module Pass.RMCO where

import Pass.State
import Relude
import Structs


-- | Removes complex operands in HAdd
rmCHAdd :: Uniq m => HAdd -> m Exp
rmCHAdd expr@TAdd{} = do
  case expr of
    TAdd (HInt _) (HInt _) -> return $ HAdd expr
    TAdd (HVar _) (HVar _) -> return $ HAdd expr
    TAdd (HInt _) (HVar _) -> return $ HAdd expr
    TAdd (HVar _) (HInt _) -> return $ HAdd expr

    TAdd i@(HInt _) e -> do
      nv <- genNewVar
      e1 <- rmCExp e
      HLet <$> rmCHLet (TLet nv e1 $ HAdd $ TAdd i $ HVar nv)
    TAdd e i@(HInt _) -> do
      nv <- genNewVar
      e1 <- rmCExp e
      HLet <$> rmCHLet (TLet nv e1 $ HAdd $ TAdd (HVar nv) i)
    TAdd v@(HVar _) e -> do
      nv <- genNewVar
      e1 <- rmCExp e
      HLet <$> rmCHLet (TLet nv e1 $ HAdd $ TAdd v $ HVar nv)
    TAdd e v@(HVar _) -> do
      nv <- genNewVar
      e1 <- rmCExp e
      HLet <$> rmCHLet (TLet nv e1 $ HAdd $ TAdd (HVar nv) v)
    TAdd e1 e2 -> do
      nv1 <- genNewVar
      nv2 <- genNewVar
      HLet <$> (rmCHLet . TLet nv1 e1 . HLet
            =<< rmCHLet ( TLet nv2 e2 $ HAdd $ TAdd (HVar nv1) $ HVar nv2))

-- | Checks if expression in `HLet` is var or int
isLetExpAtom :: HLet -> Bool
isLetExpAtom (TLet _ expr _) =
  case expr of
    HAdd e   -> isAddAtom e
    HSub e   -> isSubAtom e
    _        -> False

-- | Checks if expression in `HAdd` is var or int
isAddAtom :: HAdd -> Bool
isAddAtom (TAdd e1 e2) = isAt e1 && isAt e2
 where isAt (HInt _) = True
       isAt (HVar _) = True
       isAt _        = False

-- | Checks if expression in `HSub` is var or int
isSubAtom :: HSub -> Bool
isSubAtom (TSub (HInt _)) = True
isSubAtom (TSub (HVar _)) = True
isSubAtom _ = False

-- | Removes complex operands in HSub
rmCHSub :: Uniq m => HSub -> m HSub
rmCHSub s@(TSub expr) =
  case expr of
    HInt _ -> return s
    HVar _ -> return s
    e      -> TSub <$> rmCExp e

-- | Removes complex operands in HLet
rmCHLet :: Uniq m => HLet -> m HLet
rmCHLet l@(TLet v expr body) =
  case expr of
    HAdd (TAdd e1 e2) ->
      if isLetExpAtom l
      then return l
      else do
          nv1 <- genNewVar
          nv2 <- genNewVar
          rmCHLet . TLet nv1 e1 . HLet
             =<< rmCHLet (TLet nv2 e2
                        $ HLet $ TLet v
                         (HAdd $ TAdd (HVar nv1) $ HVar nv2) body)
    HSub (TSub e) ->
      if isLetExpAtom l
      then return l
      else do
        nv1 <- genNewVar
        rmCHLet . TLet nv1 e . HLet $ TLet v (HSub $ TSub $ HVar nv1) body

    -- TODO: add processing of HLet
    _ -> return l

-- | Removes complex operands in Exp
rmCExp :: Uniq m => Exp -> m Exp
rmCExp expr =
  case expr of
    HAdd  v -> rmCHAdd v
    HSub  v -> rmCHSub v <&> HSub
    HLet  v -> rmCHLet v <&> HLet
    _       -> return expr

-- | Removes complex operands
rmCO :: Uniq m => HolyLisp -> m HolyLisp
rmCO (HolyLisp m expr) = HolyLisp m <$> rmCExp expr
