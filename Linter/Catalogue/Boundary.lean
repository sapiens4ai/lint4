import Linter.Domain

namespace Linter.Catalogue.Boundary

open Linter

def keyword (text : String) : Piece :=
  .exact (.word .identifier text)

def ffi : Rule where
  id := ⟨.boundary, "ffi"⟩
  why := "A foreign import without its JavaScript module, or JavaScript without its PureScript module, breaks the FFI boundary"
  fix := "Keep every foreign module as a .purs and .js pair with the same name"
  checks := [
    { detector := .fixed .warning (.ffiPairing [keyword "foreign", keyword "import", .exact (.kind .identifier)] (.word .identifier "data"))
      examples := [{ bad := [⟨"src/Clock.purs", "module Clock where\nforeign import now :: Effect Number\n"⟩],
                     good := [⟨"src/Clock.purs", "module Clock where\nforeign import now :: Effect Number\nforeign import data Handle :: Type\n"⟩,
                              ⟨"src/Clock.js", "export const now = () => Date.now();\n"⟩,
                              ⟨"src/Types.purs", "module Types where\nforeign import data Handle :: Type\n"⟩] },
                   { bad := [⟨"src/Stray.js", "export const stray = 1;\n"⟩],
                     good := [⟨"src/Pair.purs", "module Pair where\nforeign import pair :: Int\n"⟩, ⟨"src/Pair.js", "export const pair = 1;\n"⟩] }] }]

def importing (language : Language) (fragment : String) : Matcher :=
  .tokens [language] .anywhere [.lineStart (.word .identifier "import"), .gap 1, .exact (.containing .identifier fragment)]

def json : Rule where
  id := ⟨.boundary, "json"⟩
  why := "Json handling spread through the code ties the domain to its wire format"
  fix := "Decode Json once at the boundary into domain types"
  checks := [
    { detector := .fixed .couldBeBetter (importing .haskell "Data.Aeson")
      examples := [{ bad := [⟨"Bad.hs", "module Bad where\nimport qualified Data.Aeson as A\n"⟩],
                     good := [⟨"Good.hs", "module Good where\nimport Data.Map (Map)\nformat = \"Data.Aeson\"\n"⟩] }] },
    { detector := .fixed .couldBeBetter (importing .purescript "Data.Argonaut")
      examples := [{ bad := [⟨"Bad.purs", "module Bad where\nimport Data.Argonaut.Core (Json)\n"⟩],
                     good := [⟨"Good.purs", "module Good where\nimport Data.Maybe (Maybe)\n"⟩] }] }]

def rules : List Rule := [ffi, json]

end Linter.Catalogue.Boundary
