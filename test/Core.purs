module Test.Core (spec) where

import Prelude

import Core
  ( ApplyErr(..)
  , Node
  , Expr(..)
  , ReplaceErr(..)
  , applyLambda
  , eqNode
  , replaceNode
  )
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

var :: String -> String -> Node
var id name = { id: id, expr: Var name }

lambda :: String -> String -> Node -> Node
lambda id param body = { id: id, expr: Lambda param body }

apply' :: String -> Node -> Node -> Node
apply' id n1 n2 = { id: id, expr: Apply n1 n2 }

spec :: Spec Unit
spec = do
  describe "Core" do
     describe "Core.replaceNode" do
        it "Finds a Var node with the given id and replaces it with the given node." $ do
          replaceNode "1" (var "3" "y") (var "1" "x") `shouldSucceedWith` (var "3" "y")
        it "Traverses a Lambda subtree to find the node with the given id and replaces it with the given node." $ do
          replaceNode "2" (var "3" "y") (lambda "1" "x" (var "2" "x")) `shouldSucceedWith` (lambda "1" "x" (var "3" "y"))
          replaceNode "3" (var "6" "z") (lambda "1" "x" (lambda "2" "y" (apply' "3" (var "4" "x") (var "5" "x")))) `shouldSucceedWith` (lambda "1" "x" (lambda "2" "y" (var "6" "z") ))
        it "Traverses Apply subtrees to find the node with the given id and replaces it with the given node." $ do
          replaceNode "3" (var "5" "y") (apply' "1" (lambda "2" "y" (var "3" "x")) (var "4" "x")) `shouldSucceedWith` (apply' "1" (lambda "2" "y" (var "5" "y")) (var "4" "x"))
          replaceNode "3" (var "5" "y") (apply' "1" (var "4" "x") (lambda "2" "y" (var "3" "x"))) `shouldSucceedWith` (apply' "1" (var "4" "x") (lambda "2" "y" (var "5" "y")))
        it "Fails with NodeNotFound if there is no node with the given id in the given tree." $ do
          replaceNode "foo" (var "3" "y") (var "1" "x") `shouldFailWith` (NodeNotFound "foo")
     describe "Core.applyLambda" do
        it "Replaces every instance of the parameter in the lambda body with the argument Node." do
          -- (\x.x y) -> y
          applyLambda (lambda "1" "x" (var "2" "x")) (var "3" "y") `shouldSucceedWith` (var "3" "y")
          -- (\x.\y.x z) -> \y.z
          applyLambda (lambda "1" "x" (lambda "2" "y" (var "3" "x"))) (var "4" "z") `shouldSucceedWith` (lambda "2" "y" (var "4" "z"))
          -- (\x.\y.(x x) z) -> \y.(z z)
          applyLambda (lambda "1" "x" (lambda "2" "y" (apply' "3" (var "4" "x") (var "5" "x")))) (var "4" "z") `shouldSucceedWith` (lambda "2" "y" (apply' "3" (var "4" "z") (var "4" "z")))
        it "Fails with NotALambda if first argument is a Var node." do
          -- x
          applyLambda (var "1" "x") (var "2" "y") `shouldFailWith` NotALambda
        it "Fails with NotALambda if first argument is an Apply node." do
          -- (x y)
          applyLambda (apply' "1" (var "2" "x") (var "3" "y")) (var "2" "y") `shouldFailWith` NotALambda
     describe "Core.eqNode" do
        it "Var nodes with the same name are equal." do
           (var "1" "foo") `nodeShouldEqual` (var "2" "foo")
           (var "1" "foo") `nodeShouldNotEqual` (var "2" "bar")
        it "Lambda nodes with the same param and body are equal." do
           (lambda "1" "x" (var "2" "x")) `nodeShouldEqual` (lambda "3" "x" (var "4" "x"))
           (lambda "1" "x" (var "2" "x")) `nodeShouldNotEqual` (lambda "3" "y" (var "4" "y"))
        it "Apply nodes containing the same expressions are equal." do
           (apply' "1" (lambda "2" "x" (var "3" "x")) (var "4" "y")) `nodeShouldEqual` (apply' "1" (lambda "2" "x" (var "3" "x")) (var "4" "y"))
           (apply' "1" (lambda "2" "x" (var "3" "x")) (var "4" "y")) `nodeShouldNotEqual` (apply' "1" (lambda "2" "y" (var "3" "y")) (var "4" "z"))

shouldFailWith :: forall a b. Eq a => Eq b => Show a => Show b => Either a b -> a -> Aff Unit
shouldFailWith (Left actual) expected = actual `shouldEqual` expected
shouldFailWith (Right actual) expected = fail $ "Expected " <> show expected <> " but received " <> show actual

shouldSucceedWith :: forall a b. Eq a => Eq b => Show a => Show b => Either a b -> b -> Aff Unit
shouldSucceedWith (Right actual) expected = actual `shouldEqual` expected
shouldSucceedWith (Left actual) expected = fail $ "Expected " <> show expected <> " but received " <> show actual

nodeShouldEqual :: Node -> Node -> Aff Unit
nodeShouldEqual = assertNode eqNode

nodeShouldNotEqual :: Node -> Node -> Aff Unit
nodeShouldNotEqual = assertNode (not <<< eqNode)

assertNode :: (Node -> Node -> Boolean) -> Node -> Node -> Aff Unit
assertNode predicate n1 n2 = predicate n1 n2 `shouldEqual` true
