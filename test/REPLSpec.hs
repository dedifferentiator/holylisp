module REPLSpec where

import Data.Bifunctor
import qualified Data.Text as T
import Relude
import REPL
import Structs
import Test.Hspec

snippets :: [(T.Text, HolyLisp)]
snippets = bimap T.pack (HolyLisp ()) <$>
  [ ("(+ 5 10)", HInt 15)
  , ("(- 5)", HInt $ -5)
  , ("(let ([var 5]) var)", HInt 5)
  , ("(+5 (+ (+ 5 (let ([x (- (+ 5 6))]) (+ x (-6)))) 6))", HInt $ -1 )
  , ("(let ([x 32]) (+ (let ([x 10]) x)  x))", HInt 42)
  , ("(let ([x 32]) (+ (let ([y 15]) (+ (let ([x (-5)]) x)  y))  x))", HInt 42)
  ]


spec :: Spec
spec = do
  describe "run code" do
    it "run snippets" do
      got <- foldl' (\b (expr, expected) -> do
                       got <- evalL expr
                       if got /= expected
                       then error $ "Expected: \n\t"
                                 <> show expected
                                 <> "\nGot: \n\t"
                                 <> show got
                       else b) (pure True) snippets
      got `shouldBe` True
