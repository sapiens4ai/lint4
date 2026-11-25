import Std.Data.HashMap
import Std.Data.HashSet
import Linter.Domain

namespace Linter.Match

open Linter

structure Hit where
  start : Position
  span : Span
  snippet : String

structure LexedFile where
  file : CheckedFile
  tokens : List Token

structure Placed where
  severity : Severity
  path : String
  hit : Hit

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
  | .except shape excluded, token => fits shape token && !fits excluded token

def opening (token : Token) : Option String :=
  if token.kind != .symbol then none
  else if token.text == "(" then some ")"
  else if token.text == "[" then some "]"
  else if token.text == "{" then some "}"
  else none

def closing (token : Token) : Bool :=
  token.kind == .symbol && (token.text == ")" || token.text == "]" || token.text == "}")

def balanced : List String → Nat → List Token → Option Nat
  | [], taken, _ => some taken
  | _ :: _, _, [] => none
  | closers@(closer :: outer), taken, token :: rest =>
    match opening token with
    | some next => balanced (next :: closers) (taken + 1) rest
    | none =>
      if token.kind == .symbol && token.text == closer then balanced outer (taken + 1) rest
      else balanced closers (taken + 1) rest

def hidden : TokenKind → Bool
  | .comment | .pragma => true
  | .identifier | .symbol | .number | .text | .character => false

def code (tokens : List Token) : List Token :=
  tokens.filter fun token => !hidden token.kind

def startsDeclaration (token : Token) : Bool := token.start.column == 1

def reach : Nat → Nat → List Token → Nat
  | 0, _, _ => 0
  | _, _, [] => 0
  | limit + 1, depth, token :: rest =>
    if startsDeclaration token then 0
    else if closing token then
      match depth with
      | 0 => 0
      | outer + 1 => reach limit outer rest + 1
    else if (opening token).isSome then reach limit (depth + 1) rest + 1
    else reach limit depth rest + 1

def single (test : Token → Bool) : List Token → List Nat
  | token :: _ => if test token then [1] else []
  | [] => []

def spans : Piece → List Token → List Nat
  | .exact shape, tokens => single (fits shape) tokens
  | .anyOf options, tokens => single (fun token => options.any (fits · token)) tokens
  | .lineStart shape, tokens => single (fun token => startsDeclaration token && fits shape token) tokens
  | .many shape, tokens =>
    match (tokens.takeWhile (fits shape ·)).length with
    | 0 => []
    | run => [run]
  | .notBefore options, token :: _ => if options.any (fits · token) then [] else [0]
  | .notBefore _, [] => [0]
  | .gap limit, tokens => List.range (reach limit 0 tokens + 1)
  | .skipBalanced, token :: rest =>
    match opening token with
    | some closer => (balanced [closer] 1 rest).toList
    | none => []
  | .skipBalanced, [] => []

def sequence : Nat → List Piece → List Token → Option (List Token)
  | 0, _, _ => none
  | _ + 1, [], _ => some []
  | fuel + 1, piece :: pieces, tokens =>
    (spans piece tokens).findSome? fun count => (sequence fuel pieces (tokens.drop count)).map (tokens.take count ++ ·)

def precedes (a b : Position) : Bool := a.line < b.line || (a.line == b.line && a.column < b.column)

def joined (earlier later : List Token) : Option (List Token) :=
  match earlier.getLast?, later.head? with
  | some last, some first =>
    if precedes last.start first.start then none else some (earlier ++ later.filter fun token => precedes last.start token.start)
  | _, _ => none

def merged (matched : List Token) : List (List Token) → List (List Token)
  | previous :: older =>
    match joined previous matched with
    | some union => union :: older
    | none => matched :: previous :: older
  | [] => [matched]

def scanFrom (pieces : List Piece) (fuel : Nat) : List (List Token) → List Token → List (List Token)
  | found, [] => found.reverse
  | found, token :: rest =>
    match sequence fuel pieces (token :: rest) with
    | some matched => scanFrom pieces fuel (merged matched found) rest
    | none => scanFrom pieces fuel found rest

def scan (pieces : List Piece) (tokens : List Token) : List (List Token) :=
  scanFrom pieces (pieces.length + 1) [] tokens

inductive Section where
  | shown | concealed

def outside (header : TokenShape) (tokens : List Token) : List Token :=
  let (kept, _) := tokens.foldl (init := ([], Section.shown)) fun (kept, state) token =>
    let now := if startsDeclaration token then (if fits header token then .concealed else .shown) else state
    match now with
    | .shown => (token :: kept, now)
    | .concealed => (kept, now)
  kept.reverse

def visible : Region → List Token → List Token
  | .hidden, tokens => tokens
  | .anywhere, tokens => code tokens
  | .outside header, tokens => outside header (code tokens)

def hit (matched : List Token) : Option Hit :=
  match matched.head?, matched.getLast? with
  | some first, some last =>
    some ⟨first.start, ⟨first.start.line, last.stop.line⟩, " ".intercalate (matched.map (·.text))⟩
  | _, _ => none

def tokenHits (region : Region) (pieces : List Piece) (tokens : List Token) : List Hit :=
  (scan pieces (visible region tokens)).filterMap hit

def withoutReturn (text : String) : String :=
  if text.endsWith "\r" then String.ofList text.toList.dropLast else text

def sourceLines (file : LexedFile) : List String := (file.file.source.code.splitOn "\n").map withoutReturn

def shortened (text : String) : String :=
  if text.length > 100 then String.ofList (text.toList.take 100) ++ "..." else text

def exceeded (tiers : List Tier) (size : Nat) : Option Tier :=
  tiers.foldl (init := none) fun best tier =>
    match best with
    | some current => if size > tier.above && tier.above > current.above then some tier else best
    | none => if size > tier.above then some tier else none

def longLines (tiers : List Tier) (file : LexedFile) : List Placed :=
  (sourceLines file).zipIdx.filterMap fun (text, index) =>
    (exceeded tiers text.length).map fun tier =>
      ⟨tier.severity, file.file.source.path, ⟨⟨index + 1, 1⟩, ⟨index + 1, index + 1⟩, shortened text.trimAscii.toString⟩⟩

def byLine (tokens : List Token) : List (List Token) :=
  tokens.splitBy fun a b => a.start.line == b.start.line

def denseLines (shapes : List TokenShape) (atLeast : Nat) (file : LexedFile) : List Hit :=
  let lines := (sourceLines file).toArray
  (byLine ((code file.tokens).filter fun token => shapes.any (fits · token))).filterMap fun here =>
    match here.head? with
    | some first =>
      let line := first.start.line
      if here.length ≥ atLeast then some ⟨first.start, ⟨line, line⟩, (lines.getD (line - 1) "").trimAscii.toString⟩ else none
    | none => none

def declarations (tokens : List Token) : List (List Token) :=
  tokens.splitBy fun _ token => !startsDeclaration token

def largeDeclarations (limit : Nat) (file : LexedFile) : List Hit :=
  (declarations (code file.tokens)).filterMap fun group =>
    if group.length > limit then
      (hit group).map fun found => { found with snippet := s!"{group.length} tokens: " ++ " ".intercalate ((group.take 6).map (·.text)) }
    else none

def owners (entries : List (String × String)) : Std.HashMap String (List String) :=
  entries.foldl (init := {}) fun map (key, path) =>
    let known := map.getD key []
    if known.contains path then map else map.insert key (path :: known)

def pathOf (file : LexedFile) : String := file.file.source.path

def firstOfEach (tokens : List Token) : List Token :=
  let (kept, _) := tokens.foldl (init := ([], (∅ : Std.HashSet String))) fun (kept, seen) token =>
    if seen.contains token.text then (kept, seen) else (token :: kept, seen.insert token.text)
  kept.reverse

def repeated (minModules minLength : Nat) (ignored : List String) (files : List LexedFile) : List (String × Hit) :=
  let found := files.map fun file =>
    (pathOf file, (code file.tokens).filter fun token =>
      token.kind == .number && token.text.length ≥ minLength && !ignored.contains token.text)
  let places := owners (found.flatMap fun (path, tokens) => tokens.map fun token => (token.text, path))
  found.flatMap fun (path, tokens) =>
    (firstOfEach tokens).filterMap fun token =>
      let modules := places.getD token.text []
      if modules.length ≥ minModules then
        (hit [token]).map fun found => (path, { found with snippet := s!"{token.text} appears in {modules.length} modules" })
      else none

def bodies (minTokens : Nat) (ignored : List String) (file : LexedFile) : List (String × List Token) :=
  (declarations (code file.tokens)).filterMap fun group =>
    match group with
    | name :: body =>
      if body.length ≥ minTokens && !ignored.contains name.text then some (" ".intercalate (body.map (·.text)), group) else none
    | [] => none

def duplicates (minTokens : Nat) (ignored : List String) (files : List LexedFile) : List (String × Hit) :=
  let found := files.map fun file => (pathOf file, bodies minTokens ignored file)
  let places := owners (found.flatMap fun (path, groups) => groups.map fun (key, _) => (key, path))
  found.flatMap fun (path, groups) => groups.filterMap fun (key, group) =>
    match (places.getD key []).find? (· != path), hit group, group.head? with
    | some other, some found, some name => some (path, { found with snippet := s!"{name.text} repeats a declaration in {other}" })
    | _, _, _ => none

def orphanHit : Discovered → List (String × Hit)
  | .orphan path => [(path, ⟨⟨1, 1⟩, ⟨1, 1⟩, path⟩)]
  | .checked _ | .unreadable _ _ => []

def unpaired (foreign : List Piece) (exempt : TokenShape) (codebase : Codebase) (files : List LexedFile) : List (String × Hit) :=
  let hosts := files.filter (·.file.language == .purescript)
  let missing := hosts.flatMap fun file =>
    let imports := (scan foreign (code file.tokens)).filter fun matched => !(matched.getLast?.any (fits exempt ·))
    match (imports.filterMap hit).head? with
    | some found => if codebase.paired.contains (pathOf file) then [] else [(pathOf file, found)]
    | none => []
  missing ++ codebase.found.flatMap orphanHit

def within (languages : List Language) (files : List LexedFile) : List LexedFile :=
  files.filter fun file => languages.contains file.file.language

def eachFile (languages : List Language) (files : List LexedFile) (find : LexedFile → List Hit) : List (String × Hit) :=
  (within languages files).flatMap fun file => (find file).map (pathOf file, ·)

def matcherHits (codebase : Codebase) : Matcher → List LexedFile → List (String × Hit)
  | .tokens languages region pieces, files => eachFile languages files fun file => tokenHits region pieces file.tokens
  | .lineDensity languages shapes atLeast, files => eachFile languages files (denseLines shapes atLeast)
  | .declarationTokens languages limit, files => eachFile languages files (largeDeclarations limit)
  | .repeatedLiteral languages minModules minLength ignored, files => repeated minModules minLength ignored (within languages files)
  | .duplicateBody languages minTokens ignored, files => duplicates minTokens ignored (within languages files)
  | .ffiPairing foreign exempt, files => unpaired foreign exempt codebase files

def fileScoped : Matcher → Bool
  | .tokens .. | .lineDensity .. | .declarationTokens .. => true
  | .repeatedLiteral .. | .duplicateBody .. | .ffiPairing .. => false

def runsIn : Mode → Detector → Bool
  | .full, .fixed _ _ => true
  | .full, .lineLength _ _ => true
  | .diff, .fixed _ matcher => fileScoped matcher
  | .diff, .lineLength _ _ => true

def detectorHits (codebase : Codebase) : Detector → List LexedFile → List Placed
  | .fixed severity matcher, files => (matcherHits codebase matcher files).map fun (path, found) => ⟨severity, path, found⟩
  | .lineLength languages tiers, files => (within languages files).flatMap (longLines tiers)

end Linter.Match
