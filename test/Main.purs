module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Parse as Parse
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Term as Term

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        Parse.spec
        Term.spec
