module PassesSpec where

import Pass.Pass
import Pass.State
import Relude
import Structs
import Test.Hspec

uniquifyData :: [(HolyLisp, HolyLisp)]
uniquifyData =
  [ ( HolyLisp () (HLet (TVar "x") (HInt 32) (HAdd (HLet (TVar "x") (HInt 10) (HVar (TVar "x"))) (HVar (TVar "x"))))
    , HolyLisp () (HLet (TVar "x0") (HInt 32) (HAdd (HLet (TVar "x1") (HInt 10) (HVar (TVar "x1"))) (HVar (TVar "x0")))))
  , ( HolyLisp () (HAdd (HInt 5) (HAdd (HAdd (HInt 5) (HLet (TVar "x") (HSub (HAdd (HInt 5) (HInt 6))) (HAdd (HVar (TVar "x")) (HInt 6)))) (HInt 6)))
    , HolyLisp () (HAdd (HInt 5) (HAdd (HAdd (HInt 5) (HLet (TVar "x0") (HSub (HAdd (HInt 5) (HInt 6))) (HAdd (HVar (TVar "x0")) (HInt 6)))) (HInt 6))))
  , ( HolyLisp () (HLet (TVar "x") (HInt 32) (HAdd (HLet (TVar "y") (HInt 15) (HAdd (HLet (TVar "x") (HInt 5) (HVar (TVar "x"))) (HVar (TVar "y")))) (HVar (TVar "x0"))))
    , HolyLisp () (HLet (TVar "x0") (HInt 32) (HAdd (HLet (TVar "y1") (HInt 15) (HAdd (HLet (TVar "x2") (HInt 5) (HVar (TVar "x2"))) (HVar (TVar "y1")))) (HVar (TVar "x0"))))
    )
  ]

spec :: Spec
spec = do
  describe "Passes" do
    it "uniquify" do
      let uniq = runUniq 0
      got <- foldlM (\_ (src, expected) -> do
                              res <- fst <$> uniq (uniquify src)
                              if (== expected) res
                              then return True
                              else error $ "Expected: \n\t"
                                    <> show expected
                                    <> "\nGot: \n\t"
                                    <> show res
                    ) True uniquifyData

      got `shouldBe` True

