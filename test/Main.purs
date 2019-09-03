module Test.Main where

import Prelude

import Test.Core as Core
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Effect (Effect)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  Core.spec
