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
    , "parsing"
    , "psci-support"
    , "quickcheck"
    , "spec"
    , "spec-quickcheck"
    , "uuid"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
