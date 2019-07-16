let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/master/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/master/src/packages.dhall sha256:829763a062e0f605cace2dbbf73776715930e23e66345c4de61b90db44331159

let overrides = {=}

let additions =
      { jquery =
          mkPackage
          [ "web-dom", "effect", "foreign" ]
          "https://github.com/purescript-contrib/purescript-jquery.git"
          "v5.0.0"
      }

in  upstream // overrides // additions
