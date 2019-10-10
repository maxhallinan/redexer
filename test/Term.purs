module Test.Term (spec) where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Parse (parse)
import Term (step)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "Term" do
    describe "Term.step" do
      it "Does nothing when term is in normal form." do
        "x" `shouldStepTo` "x"
        "λx.x" `shouldStepTo` "(λx.x)"
        "λx.λy.λz. x y z" `shouldStepTo` "(λx.(λy.(λz.((x y) z))))"
        "x y" `shouldStepTo` "(x y)"
      it "Substitutes argument for all occurences of function parameter." do
        "(λx.x) y" `shouldStepTo` "y"
        "(λx.x x z x) y" `shouldStepTo` "(((y y) z) y)"
      it "Does not substitute where outer parameter is shadowed by inner parameter." do
        "(λx.λx.x) y" `shouldStepTo` "(λx.x)"
      it "Avoids variable capture." do
        "(λx.(λy.x y) z) y" `shouldStepTo` "((λy.(y1 y)) z)"
        "(λx.(λy.x y) z) λy.y" `shouldStepTo` "((λy.((λy1.y1) y)) z)"
        "(λx.(λy.λz.x y z) z) λy.y" `shouldStepTo` "((λy.(λz.(((λy1.y1) y) z))) z)"
      it "Can be run repeatedly to find the term's normal form." do
        "(λx.λy.λs.λz.(x s) (y s z)) (λs.λz.s z) (λs.λz.s (s z))" `hasNormalForm` "(λs.(λz.(s (s (s z)))))"

shouldStepTo :: String -> String -> Aff Unit
shouldStepTo input expected = case parse input of
  Left err -> fail $ "Parsing \"" <> input <> "\" failed with " <> (show err)
  Right term -> (show $ step term) `shouldEqual` expected

hasNormalForm :: String -> String -> Aff Unit
hasNormalForm input expected = case parse input of
  Left err -> fail $ "Parsing \"" <> input <> "\" failed with " <> (show err)
  Right term -> (show $ go term) `shouldEqual` expected
  where
  go x =
    let
      next = step x
    in
      if (show next) == (show x) then
        x
      else
        go next
