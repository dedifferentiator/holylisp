module CMD.CMD
  ( optionsP
  , Options (..)
  ) where

import Options.Applicative
import Relude

-- | Options for showing the state of AST
-- for each of compiler passes
data ShowAst = ShowAst
  { shRaw      :: Bool -- ^ show raw ast
  , shUniquify :: Bool -- ^ show ast after uniquify pass
  , shRMCO     :: Bool -- ^ show ast after RMCO pass
  } deriving (Eq, Generic, Show)

-- | Level of optimisation
data OptLevel = None
              | O1
              deriving (Eq, Generic, Show)

-- | Command-line flags
data Options = Options
  { optim :: OptLevel
  , ast   :: ShowAst
  } deriving (Eq, Generic, Show)

-- | `Options` parser
optionsP :: Parser Options
optionsP = Options <$> (optLevelP <|> pure None) <*> showAstP

-- | `ShowAst` parser
showAstP :: Parser ShowAst
showAstP = ShowAst
  <$> switch
      ( long "raw-ast"
     <> help "show raw AST")
  <*> switch
      ( long "uniq-ast"
     <> help "show AST after uniquify pass")
  <*> switch
      ( long "rmco-ast"
     <> help "show AST after RMCO pass")

-- | `OptLevel` parser
optLevelP :: Parser OptLevel
optLevelP = flag' O1
      ( long "O1"
     <> help "set optimisation level at O1")
