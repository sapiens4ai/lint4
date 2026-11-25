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

def ownDomains : List String := [".".intercalate ["sapiens4", "ai"]]

def reservedHosts : List String := [".invalid", ".local", ".test", ".example", "example.com"]

def namespaces : List String := ["http://www.w3.org/", "http://www.apple.com/DTDs/", "file://"]

def bareSchemes : List String := ["\"https://\"", "\"http://\""]

def hostEdges : List String := ["\"", "'", "`", " ", "/", ".", "@", "(", "="]

def hostEnds : List String := ["\"", "'", "`", " ", "/", ":", "?", "#", ",", ";", ")", "]", "$", "{", "}", "\\"]

def bounded (host : String) : List TokenShape :=
  let edges := if host.startsWith "." then [""] else hostEdges
  edges.flatMap fun edge => hostEnds.map fun ending => .containing .text (edge ++ host ++ ending)

def withoutAny (shape : TokenShape) (excluded : List TokenShape) : TokenShape :=
  excluded.foldl .except shape

def addressBefore : List String := ["\"", "'", "`", "://", "@", "."]

def addressAfter : List String := ["\"", "'", "`", "/", ":", "?", "#"]

def addressed (host : String) : List TokenShape :=
  addressBefore.flatMap fun edge => addressAfter.map fun ending => .containing .text (edge ++ host ++ ending)

def ownHosts : List TokenShape := ownDomains.flatMap addressed

def address : TokenShape :=
  withoutAny (.containing .text "://") (ownHosts ++ reservedHosts.flatMap bounded ++ namespaces.map (.containing .text) ++ bareSchemes.map (.word .text))

def own : String := ownDomains.headD ""

def hardcodedEndpoint : Rule where
  id := ⟨.boundary, "hardcoded-endpoint"⟩
  why := "A hard-coded address ties the code to one environment and hides configuration"
  fix := "Read the address from configuration or the environment, without a literal default in code"
  checks := [
    { detector := .fixed .noGo (.tokens Language.all .anywhere [.anyOf ownHosts])
      examples := [{ bad := [⟨"Telemetry.hs", s!"module Telemetry where\nendpoint = maybe \"https://telemetry.{own}\" id <$> lookupEnv \"ORIGIN\"\n"⟩],
                     good := [⟨"Telemetry.hs", "module Telemetry where\nendpoint = lookupEnv \"SAPIENS_TELEMETRY_ORIGIN\"\n"⟩,
                              ⟨"Brand.purs", s!"module Brand where\nname = \"docs.{own}d\"\nwords = \"Sapiens4, relay, {own}, Whisper.\"\n"⟩,
                              ⟨"Footer.purs", s!"module Footer where\nfooter = link origin \"{own} ↗\"\nnote = \"visit {own}/help\"\n"⟩] },
                   { bad := [⟨"Relay.purs", "module Relay where\nforeign import relay :: String\n"⟩,
                             ⟨"Relay.js", s!"export const relay = \"remote-relay.{own}\";\n"⟩,
                             ⟨"Mail.hs", s!"module Mail where\nsupport = \"help@{own}\"\n"⟩,
                             ⟨"Bare.purs", s!"module Bare where\nhost = \"{own}:443\"\n"⟩],
                     good := [⟨"Relay.purs", "module Relay where\nforeign import relay :: String\n"⟩,
                              ⟨"Relay.js", "export const relay = process.env.RELAY_ORIGIN;\n"⟩] }] },
    { detector := .fixed .warning (.tokens Language.all .anywhere [.exact address])
      examples := [{ bad := [⟨"Host.lean", "def origin (port : Nat) : String := s!\"http://127.0.0.1:{port}\"\n"⟩,
                             ⟨"Update.hs", "module Update where\nreleases = \"https://api.github.com/repos/\"\n"⟩],
                     good := [⟨"Svg.purs", "module Svg where\nnamespace = \"http://www.w3.org/2000/svg\"\n"⟩,
                              ⟨"Origin.lean", "def hostile : String := \"Origin: https://untrusted.invalid\"\n"⟩,
                              ⟨"Probe.hs", "module Probe where\nprobe = [\"https://invalid.local\", \"http://relay.test:9\"]\n"⟩,
                              ⟨"Docs.hs", "module Docs where\ndocs = \"https://api.example.com/v1\"\n"⟩] }] }]

def untypedFfi : Rule where
  id := ⟨.boundary, "untyped-ffi"⟩
  why := "A foreign import typed as Json, Foreign or a free type variable lets JavaScript return anything unchecked"
  fix := "Give the import a concrete type, or decode its result at the boundary"
  checks := [
    { detector := .fixed .couldBeBetter (.tokens [.purescript] .code [.lineStart (.word .identifier "foreign"), keyword "import",
        .exact (.except (.kind .identifier) (.word .identifier "data")), .exact (.word .symbol "::"), .gap 12,
        .anyOf [.segment "Foreign", .segment "Json", .word .identifier "forall"]])
      examples := [{ bad := [⟨"Bad.purs", "module Bad where\nforeign import call :: String -> Effect Json\n"⟩,
                             ⟨"Bad.js", "export const call = (n) => () => n;\n"⟩,
                             ⟨"Any.purs", "module Any where\nforeign import cast :: forall a b. a -> b\n"⟩, ⟨"Any.js", "export const cast = (a) => a;\n"⟩],
                     good := [⟨"Good.purs", "module Good where\nforeign import now :: Effect Number\nforeign import data Json :: Type\n" ++
                       "hint = \"foreign import x :: Json\"\n"⟩, ⟨"Good.js", "export const now = () => Date.now();\n"⟩] }] }]

def keyOperators : Piece := .anyOf [.word .symbol ".:", .word .symbol ".:?", .word .symbol ".:!"]

def lookups : Piece := .anyOf (["lookup", "findWithDefault", "member"].map .segment)

def decoders : Region := .outside (.word .identifier "instance")

def jsonKeyLookup : Rule where
  id := ⟨.boundary, "json-key-lookup"⟩
  why := "Looking up string keys in the middle of the code spreads the wire format through the domain"
  fix := "Decode the object once in a FromJSON or DecodeJson instance and work with the decoded type"
  checks := [
    { detector := .fixed .couldBeBetter (.tokens [.haskell, .purescript] decoders [keyOperators, .exact (.kind .text)])
      examples := [{ bad := [⟨"Bad.hs", "userName o = o .: \"name\"\n"⟩, ⟨"Bad.purs", "nick o = o .:? \"nick\"\n"⟩],
                     good := [⟨"Good.hs", "instance FromJSON User where\n  parseJSON = withObject \"User\" $ \\o -> User <$> o .: \"name\"\n" ++
                       "hint = \"o .: \\\"name\\\"\"\n"⟩] }] },
    { detector := .fixed .couldBeBetter (.tokens [.haskell, .purescript] decoders [lookups, .exact (.except (.kind .text) (.word .text "\"\"")),
        .exact (.kind .identifier)])
      examples := [{ bad := [⟨"Bad.hs", "userName o = KM.lookup \"name\" o\n"⟩],
                     good := [⟨"Good.hs", "userName key o = KM.lookup key o\n" ++
                       "instance FromJSON User where\n  parseJSON = withObject \"User\" $ \\o -> maybe empty pure (KM.lookup \"name\" o)\n"⟩] }] }]

def rules : List Rule := [ffi, json, hardcodedEndpoint, untypedFfi, jsonKeyLookup]

end Linter.Catalogue.Boundary
