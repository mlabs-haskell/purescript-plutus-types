{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aeson"
  , "aff"
  , "argonaut-codecs"
  , "arrays"
  , "bifunctors"
  , "bytearrays"
  , "cardano-plutus-data-schema"
  , "cardano-data-lite"
  , "cardano-types"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "encoding"
  , "exceptions"
  , "foldable-traversable"
  , "gen"
  , "js-bigints"
  , "lattice"
  , "maybe"
  , "monad-logger"
  , "mote"
  , "mote-testplan"
  , "newtype"
  , "nonempty"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "quickcheck"
  , "quickcheck-combinators"
  , "spec"
  , "spec-quickcheck"
  , "strings"
  , "these"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
