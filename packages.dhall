let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8/packages.dhall sha256:0e95ec11604dc8afc1b129c4d405dcc17290ce56d7d0665a0ff15617e32bbf03

-- These overrides can be removed when the package set is next updated.
let overrides = 
  { argonaut = upstream.argonaut // { version = "v7.0.0" } 
  , argonaut-codecs = upstream.argonaut-codecs // { version = "v7.0.0" }
  , argonaut-generic = upstream.argonaut-generic // { version = "v6.0.0" }
  , argonaut-traversals = upstream.argonaut-traversals // { version = "v8.0.0" }
  , trout = upstream.trout // { version = "v0.12.3" }
  }

let additions = {=}

in  upstream // overrides // additions
