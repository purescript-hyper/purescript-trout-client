{ name =
    "trout-client"
, dependencies =
    [ "affjax"
    , "argonaut-generic"
    , "hyper"
    , "hypertrout"
    , "jquery"
    , "node-http"
    , "prelude"
    , "psci-support"
    , "trout"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
