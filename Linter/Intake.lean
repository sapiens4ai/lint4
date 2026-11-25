import Linter.Domain

namespace Linter.Intake

open Linter System

structure Foreign where
  script : String
  host : String
  language : Language

inductive Marker where
  | header (lines : Nat) (phrases : List (List String))
  | minified (maxLines : Nat) (longest : Nat)

structure Policy where
  vendored : List String
  foreign : Foreign
  generated : List Marker

inductive DiffSource where
  | stdin
  | file (path : FilePath)

inductive Input where
  | archive (path : FilePath)
  | diff (source : DiffSource)

inductive IntakeError where
  | unpack (archive : FilePath) (detail : String)
  | unreadable (source : String) (detail : String)
  | malformed (detail : String)
  | empty
  | coloured
  | unchanged

def IntakeError.message : IntakeError → String
  | .unpack archive detail => s!"cannot unpack {archive}: {detail}"
  | .unreadable source detail => s!"cannot read {source}: {detail}"
  | .malformed detail => detail
  | .empty => "the diff is empty: nothing to check"
  | .coloured => "the diff contains terminal colour codes: produce it with git diff --no-color --unified=100000"
  | .unchanged => "no changed file found: send a unified diff with hunks, produced by git diff --no-color --unified=100000"

inductive Kind where
  | source (language : Language)
  | orphan

def relative (root path : FilePath) : String :=
  (path.toString.drop (root.toString.length + 1)).toString

def sibling (path extension : String) : String := ((System.FilePath.mk path).withExtension extension).toString

def kindOf (foreign : Foreign) (paths : List String) (path : String) : Option Kind :=
  match (System.FilePath.mk path).extension with
  | some extension =>
    if extension == foreign.script then
      if paths.contains (sibling path foreign.host) then some (.source foreign.language) else some .orphan
    else (Language.ofExtension extension).map .source
  | none => none

def marks (code : String) : Marker → Bool
  | .header count phrases =>
    let head := String.intercalate "\n" ((code.splitOn "\n").take count)
    phrases.all fun options => options.any fun phrase => (head.splitOn phrase).length > 1
  | .minified maxLines longest =>
    let lines := (code.trimAsciiEnd.toString.splitOn "\n")
    lines.length ≤ maxLines && lines.any (·.length ≥ longest)

def place (policy : Policy) (kind : Kind) (source : SourceFile) : Option Discovered :=
  if policy.generated.any (marks source.code) then none
  else match kind with
    | .source language => some (.checked ⟨language, source⟩)
    | .orphan => some (.orphan source.path)

def pairedHosts (foreign : Foreign) (paths : List String) : List String :=
  paths.filter fun path =>
    (System.FilePath.mk path).extension == some foreign.host && paths.contains (sibling path foreign.script)

def discover (policy : Policy) (files : List SourceFile) : Codebase :=
  let paths := files.map (·.path)
  ⟨files.filterMap fun file => (kindOf policy.foreign paths file.path).bind (place policy · file), pairedHosts policy.foreign paths⟩

def decoded (bytes : ByteArray) : Except String String :=
  match String.fromUTF8? bytes with
  | some text => .ok text
  | none => .error "file is not valid UTF-8 text"

def readSource (root path : FilePath) : IO (Except String String) := do
  match ← (IO.FS.readBinFile path).toBaseIO with
  | .ok bytes => pure (decoded bytes)
  | .error problem => pure (.error ((toString problem).replace (root.toString ++ "/") ""))

def found (policy : Policy) (root : FilePath) (names : List String) (path : FilePath) : IO (List Discovered) := do
  let name := relative root path
  match kindOf policy.foreign names name with
  | none => pure []
  | some kind =>
    match ← readSource root path, kind with
    | .ok code, _ => pure (place policy kind ⟨name, code⟩).toList
    | .error message, .source language => pure [.unreadable language ⟨name, message⟩]
    | .error _, .orphan => pure [.orphan name]

def collect (policy : Policy) (root : FilePath) : IO Codebase := do
  let paths ← root.walkDir fun dir => do
    let link := (← dir.symlinkMetadata).type == .symlink
    pure (!link && !(dir.fileName.any policy.vendored.contains))
  let mut plain : List FilePath := []
  for path in paths do
    if (← path.symlinkMetadata).type != .symlink then plain := path :: plain
  let names := plain.reverse.map (relative root ·)
  let discovered ← plain.reverse.flatMapM (found policy root names)
  pure ⟨discovered, pairedHosts policy.foreign names⟩

structure Changed where
  path : Option String
  lines : Array String
  added : Array Nat
  hunks : Nat

inductive Side where
  | absent
  | named (path : String)

structure Header where
  git : Option (String × String)
  old : Side

structure Reading where
  done : Array Changed
  current : Option Changed
  header : Header
  oldLeft : Nat
  newLeft : Nat

def octal (digit : Char) : Option Nat :=
  if '0' ≤ digit && digit ≤ '7' then some (digit.toNat - '0'.toNat) else none

def escaped : Char → Option UInt8
  | 'n' => some 10
  | 't' => some 9
  | 'r' => some 13
  | 'a' => some 7
  | 'b' => some 8
  | 'f' => some 12
  | 'v' => some 11
  | '"' => some 34
  | '\\' => some 92
  | _ => none

def escape : List Char → Option (UInt8 × List Char)
  | first :: second :: third :: rest =>
    match octal first, octal second, octal third with
    | some high, some middle, some low => some ((high * 64 + middle * 8 + low).toUInt8, rest)
    | _, _, _ => (escaped first).map (·, second :: third :: rest)
  | first :: rest => (escaped first).map (·, rest)
  | [] => none

def unquoted : Nat → ByteArray → List Char → Option (String × List Char)
  | 0, _, _ => none
  | _ + 1, _, [] => none
  | _ + 1, bytes, '"' :: rest => (String.fromUTF8? bytes).map (·, rest)
  | fuel + 1, bytes, '\\' :: rest => (escape rest).bind fun (byte, after) => unquoted fuel (bytes.push byte) after
  | fuel + 1, bytes, char :: rest => unquoted fuel (bytes ++ char.toString.toUTF8) rest

def quoted (chars : List Char) : Option (String × List Char) :=
  match chars with
  | '"' :: rest => unquoted chars.length .empty rest
  | _ => none

def side (line : String) : Except String Side :=
  let field := (line.drop 4).toString
  match quoted field.toList, field.startsWith "\"" with
  | some (path, _), _ => .ok (.named path)
  | none, true => .error s!"cannot decode quoted path: {line}"
  | none, false =>
    let name := (field.splitOn "\t").headD ""
    if name == "/dev/null" then .ok .absent else .ok (.named name)

def halves (chars : List Char) : Option (String × String) :=
  let middle := chars.length / 2
  match chars.drop middle with
  | ' ' :: second => if chars.length % 2 == 1 then some (String.ofList (chars.take middle), String.ofList second) else none
  | _ => none

def gitPair (line : String) : Option (String × String) :=
  let chars := (line.drop "diff --git ".length).toString.toList
  match quoted chars with
  | some (first, ' ' :: rest) => some (first, ((quoted rest).map (·.1)).getD (String.ofList rest))
  | some _ => none
  | none => halves chars

def prefixOf (path : String) : Option Char :=
  match path.toList with
  | letter :: '/' :: _ :: _ => if letter.isAlphanum then some letter else none
  | _ => none

def mnemonic (old new : String) : Bool :=
  match prefixOf old, prefixOf new with
  | some before, some after => before != after
  | _, _ => false

def prefixed (header : Header) (path : String) : Bool :=
  match header.old, header.git with
  | .named old, _ => mnemonic old path
  | .absent, some (old, new) => mnemonic old new
  | .absent, none => prefixOf path == some 'b'

def target (header : Header) : Side → Option String
  | .absent => none
  | .named path => some (if prefixed header path then (path.drop 2).toString else path)

def range (field : String) : Option (Nat × Nat) :=
  match (field.drop 1).toString.splitOn "," with
  | [start] => start.toNat?.map (·, 1)
  | [start, count] => start.toNat?.bind fun first => count.toNat?.map (first, ·)
  | _ => none

def label (changed : Changed) : String := changed.path.getD "/dev/null"

def opened (state : Reading) (line : String) : Except String Reading :=
  match state.current, line.splitOn " " with
  | some changed, _ :: old :: new :: _ =>
    match range old, range new with
    | some (oldStart, oldCount), some (newStart, newCount) =>
      if changed.hunks > 0 || oldStart > 1 || newStart > 1 then
        .error s!"{label changed}: diff lacks full context, produce it with --unified=100000"
      else .ok { state with current := some { changed with hunks := changed.hunks + 1 }, oldLeft := oldCount, newLeft := newCount }
    | _, _ => .error s!"{label changed}: malformed hunk header: {line}"
  | some changed, _ => .error s!"{label changed}: malformed hunk header: {line}"
  | none, _ => .error s!"hunk without a file header: {line}"

def kept (changed : Changed) (text : String) (isAdded : Bool) : Changed :=
  let lines := changed.lines.push text
  { changed with lines, added := if isAdded then changed.added.push lines.size else changed.added }

def body (state : Reading) (changed : Changed) (line : String) : Except String Reading :=
  let text := (line.drop 1).toString
  match line.toList with
  | ' ' :: _ => .ok { state with current := kept changed text false, oldLeft := state.oldLeft - 1, newLeft := state.newLeft - 1 }
  | [] => .ok { state with current := kept changed "" false, oldLeft := state.oldLeft - 1, newLeft := state.newLeft - 1 }
  | '+' :: _ => .ok { state with current := kept changed text true, newLeft := state.newLeft - 1 }
  | '-' :: _ => .ok { state with oldLeft := state.oldLeft - 1 }
  | '\\' :: _ => .ok state
  | _ => .error s!"{label changed}: malformed hunk line: {line}"

def closed (state : Reading) : Reading :=
  { state with done := state.done ++ state.current.toArray, current := none }

def fresh : Header := ⟨none, .absent⟩

def step (state : Reading) (line : String) : Except String Reading :=
  if state.oldLeft + state.newLeft > 0 then
    match state.current with
    | some changed => body state changed line
    | none => .error "hunk without a file header"
  else if line.startsWith "diff --git " then .ok { closed state with header := ⟨gitPair line, .absent⟩ }
  else if line.startsWith "--- " then (side line).map fun old => { closed state with header := { state.header with old } }
  else if line.startsWith "+++ " then
    (side line).map fun new => { closed state with current := some ⟨target state.header new, #[], #[], 0⟩, header := fresh }
  else if line.startsWith "@@ " then opened state line
  else .ok state

def repeatedPath (changes : List Changed) : Option String :=
  let paths := changes.filterMap (·.path)
  paths.zipIdx.findSome? fun (path, index) => if (paths.take index).contains path then some path else none

def parse (text : String) : Except String (List Changed) := do
  let state ← (text.splitOn "\n").foldlM step ⟨#[], none, fresh, 0, 0⟩
  if state.oldLeft + state.newLeft > 0 then throw "diff ends inside a hunk"
  let changes := (closed state).done.toList
  match repeatedPath changes with
  | some path => throw s!"malformed diff: {path} appears more than once"
  | none => pure changes

def changesIn (text : String) : Except IntakeError (List Changed) := do
  if text.trimAscii.isEmpty then throw .empty
  let changes ← (parse text).mapError .malformed
  if changes.any (·.hunks > 0) then pure changes
  else if (text.splitOn "\x1b[").length > 1 then throw .coloured
  else throw .unchanged

def vendoredPath (policy : Policy) (path : String) : Bool :=
  (System.FilePath.mk path).components.any policy.vendored.contains

def diffKind (foreign : Foreign) (path : String) : Option Kind :=
  match (System.FilePath.mk path).extension with
  | some extension => if extension == foreign.script then some (.source foreign.language) else (Language.ofExtension extension).map .source
  | none => none

def rebuild (policy : Policy) (changes : List Changed) : Submission :=
  let sources := changes.filterMap fun changed =>
    changed.path.bind fun path =>
      if changed.hunks == 0 || vendoredPath policy path then none
      else some (SourceFile.mk path (String.join (changed.lines.toList.map (· ++ "\n"))), changed.added.toList)
  let files := sources.flatMap fun (source, _) =>
    match (diffKind policy.foreign source.path).bind (place policy · source) with
    | some (.checked file) => [file]
    | some (.unreadable _ _) | some (.orphan _) | none => []
  .diff files (sources.map fun (source, lines) => AddedLines.mk source.path lines)

def diffText : DiffSource → IO String
  | .stdin => do (← IO.getStdin).readToEnd
  | .file path => IO.FS.readFile path

def sourceName : DiffSource → String
  | .stdin => "standard input"
  | .file path => path.toString

def unpacked (policy : Policy) (archive : FilePath) : IO (Except IntakeError Submission) :=
  IO.FS.withTempDir fun dir => do
    let out ← IO.Process.output { cmd := "unzip", args := #["-qq", archive.toString, "-d", dir.toString] }
    if out.exitCode != 0 then return .error (.unpack archive out.stderr.trimAscii.toString)
    let root ← IO.FS.realPath dir
    return .ok (.full (← collect policy root))

def read (policy : Policy) : Input → IO (Except IntakeError Submission)
  | .archive archive => do
    match ← (unpacked policy archive).toBaseIO with
    | .ok result => pure result
    | .error problem => pure (.error (.unpack archive (toString problem)))
  | .diff source => do
    match ← (diffText source).toBaseIO with
    | .ok text => pure ((changesIn text).map (rebuild policy))
    | .error problem => pure (.error (.unreadable (sourceName source) (toString problem)))

end Linter.Intake
