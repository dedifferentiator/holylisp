module Pass.Pass
  ( uniquify
  ) where

import Pass.State
import Relude hiding (exp)
import Structs


-- | Generates unique variable
genVar :: Uniq m => HVar -> m HVar
genVar (TVar var) = do
    n <- get
    put (n + 1)
    return . TVar $ var <> show n

-- | Makes all names of variables unique
uniquify :: Uniq m => HolyLisp -> m HolyLisp
uniquify (HolyLisp m expr) = HolyLisp m <$> go expr
  where
    go :: Uniq m => Exp -> m Exp
    go exp =
      case exp of
        HLet v e1 e2 -> do
          nv <- genVar v
          HLet nv e1 . replaceVar v nv <$> go e2

        HAdd e1 e2 -> HAdd <$> go e1 <*> go e2
        HSub e -> HSub <$> go e

        _ -> return exp

-- | Replaces `from` with `to` in `exp`
replaceVar :: HVar -> HVar -> Exp -> Exp
replaceVar from to exp =
  case exp of
    HVar v -> HVar $ if v == from then to else v

    HLet v e1 e2 -> HLet v (replaceVar from to e1) $ replaceVar from to e2

    HAdd (HVar v) e -> replaceVar from to e &      (HAdd . HVar $ if v == from then to else v)
    HAdd e (HVar v) -> replaceVar from to e & (flip HAdd . HVar $ if v == from then to else v)

    HSub (HVar v) -> HSub . HVar $ if from == v then to else v
    HSub e -> replaceVar from to e

    _ -> exp
