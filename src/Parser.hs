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

module Parser
  ( runHLisp
  ) where

import           Control.Monad (void)
import qualified Data.Text as T
import           Data.Void (Void)
import           Relude hiding (some)
import           Structs
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void T.Text

-- | Parses HInt
pHInt :: Parser Exp
pHInt = do
  void space
  x <- some digitChar
  let intH = fromMaybe (error "[Parser] Error: cannot read Int from value")
            $ readMaybe @Int x
  void space

  return $ HInt intH

-- | Parses HRead
pHRead :: Parser Exp
pHRead = do
  void space
  void $ char '('
  void space
  void $ string "read"
  void space
  void $ char ')'

  return $ HRead ()

-- | Parses HSub
pHSub :: Parser Exp
pHSub = do
  void space
  void $ char '('
  void space
  void $ char '-'
  (HolyLisp _ exp) <- try pExp
  void space
  void $ char ')'

  return $ HSub exp

-- | Parses HAdd
pHAdd :: Parser Exp
pHAdd = do
  void space
  void $ char '('
  void space
  void $ char '+'
  (HolyLisp _ exp1) <- try pExp
  void space
  (HolyLisp _ exp2) <- try pExp
  void space
  void $ char ')'

  return $ HAdd exp1 exp2

-- | Parses Exp
pExp :: Parser HolyLisp
pExp = HolyLisp () <$>
  (try pHInt
   <|> try pHRead
   <|> try pHSub
   <|> pHAdd)

-- | Entrypoint of the parser
runHLisp :: T.Text -> IO ()
runHLisp = parseTest $ pExp <* eof
