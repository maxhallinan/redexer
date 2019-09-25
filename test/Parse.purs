module Test.Parse(spec) where

import Prelude

import Core (ExprType(..), isExprType)
import Data.Array ((:))
import Data.Either (either, isRight)
import Data.NonEmpty ((:|))
import Data.Char.Gen (genAlpha)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Parse (parse)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, elements, oneOf, sized)

spec :: Spec Unit
spec = do
  describe "Parse" do
    describe "Parse.parse" do
      it "Parses a variable." $ do
        quickCheck prop_parseVar
      it "Parses a lambda." $ do
        quickCheck prop_parseLambda
      it "Parses a lambda application." $ do
        quickCheck prop_parseApply
      it "Parses an arbitrary expression." $ do
        quickCheck prop_parseExpr

prop_parseVar :: ArbVar -> Boolean
prop_parseVar (ArbVar s) = parsesExpectedType V s

prop_parseLambda :: ArbLambda -> Boolean
prop_parseLambda (ArbLambda s) = parsesExpectedType L s

prop_parseApply :: ArbApply -> Boolean
prop_parseApply (ArbApply s) = parsesExpectedType A s

prop_parseExpr :: ArbExpr -> Boolean
prop_parseExpr (ArbExpr s) = isRight (parse s)

parsesExpectedType :: ExprType -> String -> Boolean
parsesExpectedType exprType s =
  either (const false) (isExprType exprType <<< _.expr) $ parse s

newtype ArbExpr = ArbExpr String
derive instance eqArbExpr :: Eq ArbExpr

instance arbitraryArbExpr :: Arbitrary ArbExpr where
  arbitrary = ArbExpr <$> genExpr

genExpr :: Gen String
genExpr = sized gen
  where gen 0 = genVar
        gen size =
          let
            s = size `div` 2
            g = gen s
          in
          oneOf $ (genVar :| [genLambda g, genApply g])

newtype ArbApply = ArbApply String
derive instance eqArbApply :: Eq ArbApply

instance arbitraryArbApply :: Arbitrary ArbApply where
  arbitrary = ArbApply <$> genApply genExpr

genApply :: Gen String -> Gen String
genApply genExpr' = token $ do
  p1 <- token $ pure "("
  e1 <- genExpr'
  s  <- token $ pure " "
  e2 <- genExpr'
  p2 <- token $ pure ")"
  pure $ p1 <> e1 <> s <> e2 <> p2

newtype ArbLambda = ArbLambda String
derive instance eqArbLambda :: Eq ArbLambda

instance arbitraryArbLambda :: Arbitrary ArbLambda where
  arbitrary = ArbLambda <$> genLambda genExpr

genLambda :: Gen String -> Gen String
genLambda genExpr' = token $ do
  l <- token $ oneOf $ (pure "\\" :| [pure "λ"])
  v <- genVar
  d <- token $ pure "."
  e <- genExpr'
  pure $ l <> v <> d <> e

newtype ArbVar = ArbVar String
derive instance eqArbVar :: Eq ArbVar

instance arbitraryArbVar :: Arbitrary ArbVar where
  arbitrary = ArbVar <$> genVar

genVar :: Gen String
genVar = token $ do
  x <- genAlpha
  xs <- arrayOf genAlpha
  pure $ fromCharArray (x:xs)

token :: Gen String -> Gen String
token x = do
  w1 <- genWhiteSpace
  x' <- x
  w2 <- genWhiteSpace
  pure $ w1 <> x' <> w2

genWhiteSpace :: Gen String
genWhiteSpace = fromCharArray <$> arrayOf genWhitespaceChar

genWhitespaceChar :: Gen Char
genWhitespaceChar = elements $ ' ' :| toCharArray "\t\n\r"
