{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "argonaut-aeson-generic"
, dependencies =
  [ "argonaut"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "console"
  , "effect"
  , "foreign-object"
  , "psci-support"
  , "test-unit"
  ]
, packages = ./packages.dhall
}
