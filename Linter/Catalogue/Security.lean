import Linter.Domain

namespace Linter.Catalogue.Security

open Linter

def tagLiteral : Piece :=
  .anyOf ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ/".toList.map fun letter => .containing .text s!"<{letter}")

def htmlStringConcat : Rule where
  id := ⟨.security, "html-string-concat"⟩
  why := "Building HTML by concatenating strings lets values inject markup"
  fix := "Build the markup with an HTML library that escapes values"
  checks := [
    { detector := .fixed .warning (.tokens [.haskell, .purescript] .code [tagLiteral, .exact (.word .symbol "<>")])
      examples := [{ bad := [⟨"Bad.hs", "cell v = \"<td>\" <> v\n"⟩, ⟨"Bad.purs", "link u = \"<a href='\" <> u <> \"'>\"\n"⟩],
                     good := [⟨"Good.hs", "label = \"<td>\"\nless a = \"a < b\" <> a\narrow = \"<- here\" <> a\n"⟩] }] }]

def both (shape : TokenShape) (also : TokenShape) : TokenShape :=
  .except shape (.except shape also)

def pathLiteral : TokenShape := .except (both (.containing .text "\"/") (.containing .text "/\"")) (.word .text "\"/\"")

def encoders : List TokenShape := ["encodeURI", "encodeURIComponent", "urlEncode", "encodePathSegments"].map .segment

def urlPathConcat : Rule where
  id := ⟨.security, "url-path-concat"⟩
  why := "Concatenating a raw value into a URL path lets a slash, query or dot segment change the request"
  fix := "Encode the segment before joining it, or build the URL with a typed route"
  checks := [
    { detector := .fixed .couldBeBetter (.tokens [.haskell, .purescript] .code [.exact pathLiteral, .anyOf [.word .symbol "<>", .word .symbol "++"],
        .notBefore (encoders ++ [.kind .text]), .exact (.kind .identifier)])
      examples := [{ bad := [⟨"Bad.purs", "userUrl id = \"/api/users/\" <> id\n"⟩, ⟨"Bad.hs", "fileUrl name = \"/files/\" ++ name\n"⟩],
                     good := [⟨"Good.purs", "userUrl id = \"/api/users/\" <> encodeURIComponent id\nbin x = \".build/bin/\" <> x\n" ++
                       "base = \"/api/\" <> \"v1\"\njoin dir file = dir <> \"/\" <> file\nhint = \"\\\"/api/\\\" <> id\"\n"⟩] }] }]

def rules : List Rule := [htmlStringConcat, urlPathConcat]

end Linter.Catalogue.Security
