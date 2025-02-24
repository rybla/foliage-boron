{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "argonaut-generic"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "exists"
  , "foldable-traversable"
  , "halogen"
  , "homogeneous"
  , "identity"
  , "integers"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  , "refs"
  , "spec"
  , "st"
  , "strings"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unordered-collections"
  , "unsafe-coerce"
  , "variant"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
