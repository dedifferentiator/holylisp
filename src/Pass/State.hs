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

module Pass.State
  ( genNewVar
  , genVar
  , runUniq
  , Uniq
  , UniqApp (..)
  ) where

import Relude
import Structs

type Uniq m = MonadState Int m

newtype UniqApp a = UniqApp
  { runUniqApp :: StateT Int IO a
  } deriving (Monad, Applicative, Functor, MonadState Int)

-- | Generates unique variable
genVar :: Uniq m => HVar -> m HVar
genVar (TVar var) = do
    n <- get
    put $ n + 1
    return . TVar $ var <> show n

-- | Generates new unique variable
genNewVar :: Uniq m => m HVar
genNewVar = do
  n <- get
  put $ n + 1
  return . TVar $ "tmp" <> show n

-- | Runs Uniq state monad
runUniq :: Int -> UniqApp a -> IO (a, Int)
runUniq p pa = runStateT (runUniqApp pa) p
