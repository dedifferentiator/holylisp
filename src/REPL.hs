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
    ( repl
    ) where

import Parser (runHLisp)
import Relude

-- | Reads input line
readL :: IO Text
readL = getLine

-- TODO: remove comments starting with ';'
-- later should check for cases with ';' inside
-- strings

-- | Evaluates input
evalL :: Text -> Text
evalL = id

-- | Prints result of interpretation
printL :: Show a => a -> IO ()
printL = print

-- | Read-eval-print
rep :: IO ()
rep = readL >>= runHLisp

-- | Read-eval-print loop
repl :: IO ()
repl = rep >> repl

