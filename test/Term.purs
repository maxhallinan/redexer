module Test.Term (spec) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import ParseTerm (parse)
import Term (eval, showTerm)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "Term" do
    describe "Term.eval" do
      it "Evaluates a value." $ do
        "x" `shouldEvalTo` "x"
        "λx.x" `shouldEvalTo` "(λx.x)"
      it "Evaluates application of variable to variable." $ do
        "x y" `shouldEvalTo` "(x y)"
      it "Evaluates application of variable to function." $ do
        "(λx.x) y" `shouldEvalTo` "y"
      it "Substitutes argument for all occurences of function parameter." $ do
        "(λx.x x z x) y" `shouldEvalTo` "(((y y) z) y)"
      it "Does not substitute where outer parameter is shadowed by inner parameter." $ do
        "(λx.λx.x) y" `shouldEvalTo` "(λx.x)"
      it "Avoids variable capture." $ do
        "(λx.(λy.x y) z) y" `shouldEvalTo` "((λy.(y1 y)) z)"
        "(λx.(λy.x y) z) λy.y" `shouldEvalTo` "((λy.((λy1.y1) y)) z)"

shouldEvalTo :: String -> String -> Aff Unit
shouldEvalTo input expected =
  case parse input of
    Left err ->
      fail $ "Parsing \"" <> input <> "\" failed with " <> (show err)
    Right term ->
      (showTerm $ eval term) `shouldEqual` expected
