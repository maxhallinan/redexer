module Test.Parse (spec) where

import Prelude

import Data.Array ((:))
import Data.Either (either, isRight)
import Data.Foldable (intercalate)
import Data.NonEmpty ((:|))
import Data.Char.Gen (genAlpha, genDigitChar)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Parse (parse)
import Term (Term(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, listOf, elements, oneOf, resize, sized)

spec :: Spec Unit
spec = do
  describe "ParseTerm" do
    describe "ParseTerm.parse" do
      it "Parses a variable." $ do
        quickCheck prop_parseVar
      it "Parses a function." $ do
        quickCheck prop_parseFn
      it "Parses a lambda application." $ do
        quickCheck prop_parseApply
      it "Parses an arbitrary expression." $ do
        quickCheck prop_parseExpr

data TermType = V | F | A

derive instance eqTermType :: Eq TermType

typeOf :: Term -> TermType
typeOf t = case t of
  Var _ _ -> V
  Fn _ _ -> F
  Apply _ _ _ -> A

isTermType :: TermType -> Term -> Boolean
isTermType termType term = typeOf term == termType

prop_parseVar :: ArbVar -> Boolean
prop_parseVar (ArbVar s) = parsesExpectedType V s

prop_parseFn :: ArbFn -> Boolean
prop_parseFn (ArbFn s) = parsesExpectedType F s

prop_parseApply :: ArbApply -> Boolean
prop_parseApply (ArbApply s) = parsesExpectedType A s

prop_parseExpr :: ArbExpr -> Boolean
prop_parseExpr (ArbExpr s) = isRight (parse s)

parsesExpectedType :: TermType -> String -> Boolean
parsesExpectedType termType s =
  either (const false) (isTermType termType) $ parse s

newtype ArbExpr = ArbExpr String
derive instance eqArbExpr :: Eq ArbExpr

instance arbitraryArbExpr :: Arbitrary ArbExpr where
  arbitrary = ArbExpr <$> genTerm

genTerm :: Gen String
genTerm = sized gen
  where gen 0 = genVar
        gen size =
          let
            s = size `div` 2
            g = gen s
          in
          oneOf (genVar :| [genFn g, genApply g, termSequence s g])
        termSequence length g = map (intercalate " ") (listOf minOne g)
          where minOne = length + 1

newtype ArbApply = ArbApply String
derive instance eqArbApply :: Eq ArbApply

instance arbitraryArbApply :: Arbitrary ArbApply where
  arbitrary = ArbApply <$> genApply genTerm

genApply :: Gen String -> Gen String
genApply genTerm' = token $ do
  p1 <- token $ pure "("
  -- A function in the left position must be wrapped in parens or the term in
  -- right position will be parsed as part of the function body.
  t1 <- oneOf (genVar :| [parens $ genFn genTerm', genApply genTerm'])
  s  <- token $ pure " "
  t2 <- genTerm'
  p2 <- token $ pure ")"
  pure $ p1 <> t1 <> s <> t2 <> p2

parens :: Gen String -> Gen String
parens x = do
  p1 <- token $ pure "("
  x' <- x
  p2 <- token $ pure ")"
  pure $ p1 <> x' <> p2

newtype ArbFn = ArbFn String
derive instance eqArbFn :: Eq ArbFn

instance arbitraryArbFn :: Arbitrary ArbFn where
  arbitrary = ArbFn <$> genFn genTerm

genFn :: Gen String -> Gen String
genFn genTerm' = token $ do
  l <- token $ oneOf $ (pure "\\" :| [pure "λ"])
  v <- genVar
  d <- token $ pure "."
  e <- genTerm'
  pure $ l <> v <> d <> e

newtype ArbVar = ArbVar String
derive instance eqArbVar :: Eq ArbVar

instance arbitraryArbVar :: Arbitrary ArbVar where
  arbitrary = ArbVar <$> genVar

genVar :: Gen String
genVar = token $ do
  x <- genAlpha
  xs <- arrayOf genDigitChar
  pure $ fromCharArray (x:xs)

token :: Gen String -> Gen String
token x = do
  w1 <- genWhiteSpace
  x' <- x
  w2 <- genWhiteSpace
  pure $ w1 <> x' <> w2

genWhiteSpace :: Gen String
genWhiteSpace = fromCharArray <$> resize 3 (arrayOf genWhitespaceChar)

genWhitespaceChar :: Gen Char
genWhitespaceChar = elements $ ' ' :| toCharArray "\t\n\r"

