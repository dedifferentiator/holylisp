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

module Pass.Passes
  ( runPasses
  ) where

import Colourista.IO
import CMD.State
import Data.Generics.Internal.VL
import Data.Generics.Product
import Pass.RMCO
import Pass.State
import Pass.Uniquify
import Relude
import Structs

-- | Runs all compiler passes
runPasses :: Config m => HolyLisp -> m HolyLisp
runPasses h = do
  conf <- get
  let ast = conf ^. field @"ast"
      shu = ast  ^. field @"shUniquify"
      shr = ast  ^. field @"shRMCO"

  let uniqPass = runUniq 0 $ uniquify h
      rmcoPass = join $ runUniq
            <$> (       snd <$> uniqPass)
            <*> (rmCO . fst <$> uniqPass)

  -- print passes if required flags
  -- were given to compiler
  when shu $ liftIO $
    greenMessage "\n>>= UNIQUIFY PASS:"
    >> uniqPass >>= putStr . ("  " <>) . show . fst
    >> putText "\n"

  when shr $ liftIO $
    greenMessage "\n>>= RMCO PASS:"
    >> rmcoPass >>= putStr . ("  " <>) . show . fst
    >> putTextLn "\n"

  liftIO $ fst <$> runUniq 0 (uniquify h >>= rmCO)
