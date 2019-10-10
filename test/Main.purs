module Test.Main where

import Prelude

import Test.Parse as Parse
import Test.Term as Term
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Effect (Effect)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  Parse.spec
  Term.spec
