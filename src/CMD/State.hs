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

module CMD.State
  ( Config
  , ConfigApp (..)
  , parseCmd
  , runConfig
  ) where

import CMD.CMD
import Options.Applicative
import Relude

type Config m = (MonadIO m, MonadState Options m)

newtype ConfigApp a = ConfigApp
  { runConfigApp :: StateT Options IO a
  } deriving (Monad, Applicative, Functor, MonadState Options, MonadIO)

-- | Runs Config state monad
runConfig :: Options -> ConfigApp a -> IO (a, Options)
runConfig opt v = runStateT (runConfigApp v) opt

-- | Parse cmd flags
parseCmd :: IO Options
parseCmd = execParser opts
  where
    opts = info (optionsP <**> helper)
      $ fullDesc
     <> progDesc "holylisp compiler"
     <> header   "holylisp - the holiest of all lisps"
