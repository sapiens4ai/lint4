import Linter.Domain

namespace Linter.Match

open Linter

structure Hit where
  start : Position
  span : Span
  snippet : String

def lastSegment (text : String) : String :=
  match (text.splitOn ".").getLast? with
  | some segment => segment
  | none => text

def fits : TokenShape → Token → Bool
  | .kind kind, token => token.kind == kind
  | .word kind text, token => token.kind == kind && token.text == text
  | .segment name, token => token.kind == .identifier && lastSegment token.text == name
  | .containing kind fragment, token => token.kind == kind && (token.text.splitOn fragment).length > 1
  | .suffix ending, token => token.kind == .identifier && (lastSegment token.text).endsWith ending

def shapes : Piece → List TokenShape
  | .exact shape => [shape]
  | .anyOf options => options
  | .lineStart shape => [shape]
  | .many shape => [shape]
  | .gap _ => []
  | .skipBalanced => []

def opening (token : Token) : Option String :=
  if token.kind != .symbol then none
  else if token.text == "(" then some ")"
  else if token.text == "[" then some "]"
  else if token.text == "{" then some "}"
  else none

def balanced : Nat → List String → List Token → List Token → Option (List Token × List Token)
  | 0, _, _, _ => none
  | _, [], taken, rest => some (taken.reverse, rest)
  | _ + 1, _ :: _, _, [] => none
  | fuel + 1, closers@(closer :: outer), taken, token :: rest =>
    match opening token with
    | some next => balanced fuel (next :: closers) (token :: taken) rest
    | none =>
      if token.kind == .symbol && token.text == closer then balanced fuel outer (token :: taken) rest
      else balanced fuel closers (token :: taken) rest

def wantsComments (pieces : List Piece) : Bool :=
  pieces.any fun piece => (shapes piece).any fun
    | .kind kind => kind == .comment
    | .word kind _ => kind == .comment
    | .containing kind _ => kind == .comment || kind == .pragma
    | .segment _ => false
    | .suffix _ => false

def sequence : Nat → List Piece → List Token → Option (List Token)
  | 0, _, _ => none
  | _, [], _ => some []
  | fuel + 1, .gap limit :: pieces, tokens =>
    let attempts := (List.range (limit + 1)).filterMap fun skip =>
      if skip > tokens.length then none
      else (sequence fuel pieces (tokens.drop skip)).map (tokens.take skip ++ ·)
    attempts.head?
  | fuel + 1, .skipBalanced :: pieces, token :: rest =>
    match opening token with
    | some closer =>
      match balanced (rest.length + 1) [closer] [token] rest with
      | some (group, after) => (sequence fuel pieces after).map (group ++ ·)
      | none => none
    | none => none
  | fuel + 1, .many shape :: pieces, tokens =>
    let run := tokens.takeWhile (fits shape ·)
    if run.isEmpty then none else (sequence fuel pieces (tokens.drop run.length)).map (run ++ ·)
  | fuel + 1, .lineStart shape :: pieces, token :: rest =>
    if token.start.column == 1 && fits shape token then (sequence fuel pieces rest).map (token :: ·) else none
  | fuel + 1, piece :: pieces, token :: rest =>
    if (shapes piece).any (fits · token) then (sequence fuel pieces rest).map (token :: ·) else none
  | _, _ :: _, [] => none

def scan (pieces : List Piece) : List Token → List (List Token)
  | [] => []
  | token :: rest =>
    match sequence (rest.length + pieces.length + 2) pieces (token :: rest) with
    | some matched => matched :: scan pieces rest
    | none => scan pieces rest

def startsDeclaration (token : Token) : Bool := token.start.column == 1

def isImport (token : Token) : Bool := token.kind == .identifier && token.text == "import"

def withoutImports : Bool → List Token → List Token
  | _, [] => []
  | inside, token :: rest =>
    let now := if startsDeclaration token then isImport token else inside
    if now then withoutImports now rest else token :: withoutImports now rest

def visible : Region → List Token → List Token
  | .anywhere, tokens => tokens
  | .code, tokens => withoutImports false tokens

def hit (matched : List Token) : Option Hit :=
  match matched.head?, matched.getLast? with
  | some first, some last =>
    some ⟨first.start, ⟨first.start.line, last.stop.line⟩, " ".intercalate (matched.map (·.text))⟩
  | _, _ => none

def applies : Matcher → Language → Bool
  | .tokens languages _ _, language => languages.contains language

def matcherHits : Matcher → List Token → List Hit
  | .tokens _ region pieces, tokens =>
    let stream := if wantsComments pieces then tokens
      else tokens.filter fun token => token.kind != .comment && token.kind != .pragma
    (scan pieces (visible region stream)).filterMap hit

def fileHits (detector : Detector) (language : Language) (tokens : List Token) : List (Severity × Hit) :=
  match detector with
  | .fixed severity matcher =>
    if applies matcher language then (matcherHits matcher tokens).map (severity, ·) else []

end Linter.Match
