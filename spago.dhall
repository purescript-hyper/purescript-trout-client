{ name =
    "trout-client"
, license =
    "MPL"
, repository =
    "https://github.com/purescript-hyper/purescript-trout-client.git"
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
