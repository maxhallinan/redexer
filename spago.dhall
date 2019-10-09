{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "control"
    , "debug"
    , "effect"
    , "fixed-points"
    , "free"
    , "functors"
    , "generics-rep"
    , "halogen"
    , "matryoshka"
    , "parsing"
    , "psci-support"
    , "quickcheck"
    , "spec"
    , "spec-quickcheck"
    , "uuid"
    , "web-dom"
    , "web-events"
    , "web-html"
    , "web-uievents"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
