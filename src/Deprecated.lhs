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

> module Deprecated where

import Control.Monad.Except
import Control.Monad.State
import Data.Char
import Data.Int
import Prelude hiding (read)
import Text.Read hiding (read, get)

data Oper = Plus
          | Min
          | Mul
          | Div
          deriving (Eq, Show)

data Token = OB        -- ^ '('
           | CB        -- ^ ')'
           | Num Int64 -- ^ Int64
           | OP Oper   -- ^ Operation alike [+-*/]

type LS m = (MonadState String m, MonadError String m)

lexer :: String -> [Token]
lexer str = go str []
  where go :: String -> [Token] -> [Token]
        go (x:xs) acc = undefined

        consume :: String -> (Char, String)
        consume (x:xs) = (x, xs)

runC :: LS m => String -> m [Token]
runC ctx = undefined

-- | If the first char is ';' then consume all chars
-- until '\n' occures.
consSemicolon :: LS m => String -> m String
consSemicolon []       = return []
consSemicolon s@(x:xs) = return $
  case x of
    ';' -> go xs
    _   -> s
  where go (l:ls) =
          case l of
            '\n' -> ls
            _    -> go ls
        go _ = []

-- | Consumes all spaces and '\t' from the head of the string
-- until non-space-tab char occurs
consSpTab :: LS m => String -> m String
consSpTab []       = return []
consSpTab s@(x:xs) =
  case x of
    ' '  -> consSpTab xs
    '\t' -> consSpTab xs
    _    -> return s

-- | Consumes '(' or ')' if they are present
consBracket :: LS m => String -> m (Maybe Token, String)
consBracket []       = return (Nothing, [])
consBracket s@(x:xs) = return $
 case x of
   '(' -> (Just OB, xs)
   ')' -> (Just CB, xs)
   _   -> (Nothing, s)

-- | Consumes Int64 if present,
-- will panic if value overflows type boundary
consNum :: LS m => String -> m (Maybe Token, String)
consNum s@(x:xs) = return $
  if isDigit x
  then go xs [x]
  else (Nothing, s)
  where go :: String -> String -> (Maybe Token, String)
        go (l:ls) acc =
          if isDigit l
          then go ls $ acc <> [l]
          else (Num <$> readMaybe acc, ls)

-- | Consumes operation if present
consOper :: LS m => String -> m (Maybe Token, String)
consOper []       = return (Nothing, [])
consOper s@(x:xs) = return $
  case x of
    '+' -> (Just $ OP Plus, xs)
    '-' -> (Just $ OP Min, xs)
    '*' -> (Just $ OP Mul, xs)
    '/' -> (Just $ OP Div, xs)
    _   -> (Nothing, s)
