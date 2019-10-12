{ name =
    "redexer"
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
