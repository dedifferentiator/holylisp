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

module REPL
    ( evalL
    , repl
    ) where

import Eval
import Parser
import Pass.Passes
import Pass.State
import Relude
import Structs


-- | Reads input line
readL :: IO Text
readL = getLine

-- | Evaluates input
evalL :: Text -> IO HolyLisp
evalL = runProg . runO1 . hParse

evalT :: Text -> IO HolyLisp
evalT = (fst <$>) . runUniq 0 . (runPasses . hParse)

-- | Prints result of interpretation
printL :: Show a => a -> IO ()
printL = print

-- | Read-eval-print
rep :: IO ()
rep = readL >>= evalT >>= printL

-- | Read-eval-print loop
repl :: IO ()
repl = rep >> repl

