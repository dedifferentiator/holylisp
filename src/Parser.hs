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
  ( hParse
  , hParseTest
  ) where

import qualified Data.Text as T
import           Relude hiding (many, some)
import           Structs
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void T.Text

-- | Parses HInt
hIntP :: Parser Exp
hIntP = do
  void space
  x <- some digitChar
  let tint = fromMaybe (error "[Parser] Error: cannot read Int from value")
           $ readMaybe @Int x
  void space

  return $ HInt $ TInt tint

-- | Parses HRead
hReadP :: Parser Exp
hReadP = do
  void space
  void $ char '('
  void space
  void $ string "read"
  void space
  void $ char ')'

  return $ HRead $ TRead ()

-- | Parses HSub
hSubP :: Parser Exp
hSubP = do
  void space
  void $ char '('
  void space
  void $ char '-'
  (HolyLisp _ e) <- try expP
  void space
  void $ char ')'

  return $ HSub $ TSub e

-- | Parses HAdd
hAddP :: Parser Exp
hAddP = do
  void space
  void $ char '('
  void space
  void $ char '+'
  (HolyLisp _ e1) <- try expP
  void space
  (HolyLisp _ e2) <- try expP
  void space
  void $ char ')'

  return $ HAdd $ TAdd e1 e2

-- | Parses HVar
hVarP :: Parser HVar
hVarP = do
  void space
  var <- try $ some letterChar
  void space

  return $ TVar var

-- | Parses HLet
hLetP :: Parser Exp
hLetP = do
  void space
  void $ char '('
  void space
  void $ try $ string "let"
  void space
  void $ char '('
  void space
  void $ char '['
  void space
  var <- try hVarP
  void space
  (HolyLisp _ e) <- try expP
  void space
  void $ char ']'
  void space
  void $ char ')'
  void space
  (HolyLisp _ body) <- try expP
  void $ char ')'

  return $ HLet $ TLet var e body

-- | Parses Exp
expP :: Parser HolyLisp
expP = HolyLisp () <$>
  (try hIntP
   <|> (try hVarP <&> HVar)
   <|> try hReadP
   <|> try hSubP
   <|> try hAddP
   <|> hLetP)

-- | Parses HLisp expression
hParse :: T.Text -> HolyLisp
hParse = either (error "cannot parse expression") id . parse expP ""

-- | Runs HLisp parser
hParseTest :: T.Text -> IO ()
hParseTest = parseTest $ expP <* eof
