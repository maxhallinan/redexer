module Test.Parse(spec) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Parse" do
    it "foo bar baz" $ do
       true `shouldEqual` true
