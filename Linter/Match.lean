import Std.Data.HashMap
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

def balanced : List String → List Token → List Token → Option (List Token × List Token)
  | [], taken, rest => some (taken.reverse, rest)
  | _ :: _, _, [] => none
  | closers@(closer :: outer), taken, token :: rest =>
    match opening token with
    | some next => balanced (next :: closers) (token :: taken) rest
    | none =>
      if token.kind == .symbol && token.text == closer then balanced outer (token :: taken) rest
      else balanced closers (token :: taken) rest

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
      match balanced [closer] [token] rest with
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
    match sequence (pieces.length + 1) pieces (token :: rest) with
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

def code (tokens : List Token) : List Token :=
  tokens.filter fun token => token.kind != .comment && token.kind != .pragma

def tokenHits (region : Region) (pieces : List Piece) (tokens : List Token) : List Hit :=
  let stream := if wantsComments pieces then tokens else code tokens
  (scan pieces (visible region stream)).filterMap hit

def sourceLines (file : LexedFile) : List String := file.file.source.code.splitOn "\n"

def lineText (file : LexedFile) (line : Nat) : String :=
  match ((sourceLines file).drop (line - 1)).head? with
  | some text => text.trimAscii.toString
  | none => ""

def exceeded (tiers : List Tier) (size : Nat) : Option Tier :=
  tiers.foldl (init := none) fun best tier =>
    match best with
    | some current => if size > tier.above && tier.above > current.above then some tier else best
    | none => if size > tier.above then some tier else none

def longLines (tiers : List Tier) (file : LexedFile) : List Placed :=
  (sourceLines file).zipIdx.filterMap fun (text, index) =>
    (exceeded tiers text.length).map fun tier =>
      ⟨tier.severity, file.file.source.path, ⟨⟨index + 1, 1⟩, ⟨index + 1, index + 1⟩, s!"{text.length} characters"⟩⟩

def denseLines (shapes : List TokenShape) (atLeast : Nat) (file : LexedFile) : List Hit :=
  let fitting := (code file.tokens).filter fun token => shapes.any (fits · token)
  (fitting.map (·.start.line)).eraseDups.filterMap fun line =>
    let here := fitting.filter (·.start.line == line)
    match here.head? with
    | some first => if here.length ≥ atLeast then some ⟨first.start, ⟨line, line⟩, lineText file line⟩ else none
    | none => none

def declarations (tokens : List Token) : List (List Token) :=
  let groups := tokens.foldl (init := []) fun groups token =>
    if startsDeclaration token then [token] :: groups
    else match groups with
      | current :: rest => (token :: current) :: rest
      | [] => [[token]]
  (groups.map List.reverse).reverse

def largeDeclarations (limit : Nat) (file : LexedFile) : List Hit :=
  (declarations (code file.tokens)).filterMap fun group =>
    if group.length > limit then
      (hit group).map fun found => { found with snippet := s!"{group.length} tokens: " ++ " ".intercalate ((group.take 6).map (·.text)) }
    else none

def heavyImports (percent : Nat) (file : LexedFile) : List Hit :=
  let filled := ((sourceLines file).filter fun text => !text.all Char.isWhitespace).length
  let imports := (((code file.tokens).filter fun token => startsDeclaration token && isImport token).map (·.start.line)).eraseDups
  match imports.head?, imports.getLast? with
  | some first, some last =>
    if imports.length * 100 > percent * filled then [⟨⟨first, 1⟩, ⟨first, last⟩, s!"{imports.length} of {filled} lines are imports"⟩]
    else []
  | _, _ => []

def owners (entries : List (String × String)) : Std.HashMap String (List String) :=
  entries.foldl (init := {}) fun map (key, path) =>
    let known := map.getD key []
    if known.contains path then map else map.insert key (path :: known)

def pathOf (file : LexedFile) : String := file.file.source.path

def repeated (minModules minLength : Nat) (ignored : List String) (files : List LexedFile) : List (String × Hit) :=
  let found := files.map fun file =>
    (pathOf file, (code file.tokens).filter fun token =>
      token.kind == .number && token.text.length ≥ minLength && !ignored.contains token.text)
  let places := owners (found.flatMap fun (path, tokens) => tokens.map fun token => (token.text, path))
  found.flatMap fun (path, tokens) =>
    (tokens.map (·.text)).eraseDups.filterMap fun value =>
      let modules := places.getD value []
      if modules.length ≥ minModules then
        (tokens.find? (·.text == value)).bind (fun token => hit [token]) |>.map fun found =>
          (path, { found with snippet := s!"{value} appears in {modules.length} modules" })
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

def foreignImport : List Piece :=
  [.exact (.word .identifier "foreign"), .exact (.word .identifier "import"), .exact (.kind .identifier)]

def sibling (path extension : String) : String := ((System.FilePath.mk path).withExtension extension).toString

def unpaired (files : List LexedFile) : List (String × Hit) :=
  let paths := files.map pathOf
  files.flatMap fun file =>
    let path := pathOf file
    match file.file.language with
    | .purescript =>
      let imports := (scan foreignImport (code file.tokens)).filter fun matched => !(matched.getLast?.any (·.text == "data"))
      match (imports.filterMap hit).head? with
      | some found => if paths.contains (sibling path "js") then [] else [(path, found)]
      | none => []
    | .javascript => if paths.contains (sibling path "purs") then [] else [(path, ⟨⟨1, 1⟩, ⟨1, 1⟩, path⟩)]
    | .haskell => []
    | .lean => []

def within (languages : List Language) (files : List LexedFile) : List LexedFile :=
  files.filter fun file => languages.contains file.file.language

def eachFile (languages : List Language) (files : List LexedFile) (find : LexedFile → List Hit) : List (String × Hit) :=
  (within languages files).flatMap fun file => (find file).map (pathOf file, ·)

def matcherHits : Matcher → List LexedFile → List (String × Hit)
  | .tokens languages region pieces, files => eachFile languages files fun file => tokenHits region pieces file.tokens
  | .lineDensity languages shapes atLeast, files => eachFile languages files (denseLines shapes atLeast)
  | .declarationTokens languages limit, files => eachFile languages files (largeDeclarations limit)
  | .importShare languages percent, files => eachFile languages files (heavyImports percent)
  | .repeatedLiteral languages minModules minLength ignored, files => repeated minModules minLength ignored (within languages files)
  | .duplicateBody languages minTokens ignored, files => duplicates minTokens ignored (within languages files)
  | .ffiPairing, files => unpaired files

def fileScoped : Matcher → Bool
  | .tokens .. | .lineDensity .. | .declarationTokens .. | .importShare .. => true
  | .repeatedLiteral .. | .duplicateBody .. | .ffiPairing => false

def runsIn : Mode → Detector → Bool
  | .full, .fixed _ _ => true
  | .full, .lineLength _ _ => true
  | .diff, .fixed _ matcher => fileScoped matcher
  | .diff, .lineLength _ _ => true

def detectorHits : Detector → List LexedFile → List Placed
  | .fixed severity matcher, files => (matcherHits matcher files).map fun (path, found) => ⟨severity, path, found⟩
  | .lineLength languages tiers, files => (within languages files).flatMap (longLines tiers)

end Linter.Match
