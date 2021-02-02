module Pass.State
  ( runUniq
  , Uniq
  , UniqApp (..)
  ) where

import Relude

type Uniq m = MonadState Int m

newtype UniqApp a = UniqApp
  { runUniqApp :: StateT Int IO a
  } deriving (Monad, Applicative, Functor, MonadState Int)

-- | Runs Uniq state monad
runUniq :: Int -> UniqApp a -> IO (a, Int)
runUniq p pa = runStateT (runUniqApp pa) p
