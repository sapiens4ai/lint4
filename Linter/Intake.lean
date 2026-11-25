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

def IntakeError.message : IntakeError → String
  | .unpack archive detail => s!"cannot unpack {archive}: {detail}"
  | .unreadable source detail => s!"cannot read {source}: {detail}"
  | .malformed detail => detail

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

structure Reading where
  done : Array Changed
  current : Option Changed
  oldLeft : Nat
  newLeft : Nat

def target (line : String) : Option String :=
  let name := ((line.drop 4).toString.splitOn "\t").headD ""
  if name == "/dev/null" then none
  else if name.startsWith "b/" then some (name.drop 2).toString
  else some name

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

def step (state : Reading) (line : String) : Except String Reading :=
  if state.oldLeft + state.newLeft > 0 then
    match state.current with
    | some changed => body state changed line
    | none => .error "hunk without a file header"
  else if line.startsWith "+++ " then
    .ok { state with done := state.done ++ state.current.toArray, current := some ⟨target line, #[], #[], 0⟩ }
  else if line.startsWith "@@ " then opened state line
  else .ok state

def parse (text : String) : Except String (List Changed) := do
  let state ← (text.splitOn "\n").foldlM step ⟨#[], none, 0, 0⟩
  if state.oldLeft + state.newLeft > 0 then throw "diff ends inside a hunk"
  pure (state.done ++ state.current.toArray).toList

def vendoredPath (policy : Policy) (path : String) : Bool :=
  (System.FilePath.mk path).components.any policy.vendored.contains

def rebuild (policy : Policy) (changes : List Changed) : Submission :=
  let present := changes.filterMap (·.path)
  let sources := changes.filterMap fun changed =>
    changed.path.bind fun path =>
      if changed.hunks == 0 || vendoredPath policy path then none
      else some (SourceFile.mk path (String.join (changed.lines.toList.map (· ++ "\n"))), changed.added.toList)
  let files := sources.flatMap fun (source, _) =>
    match (kindOf policy.foreign present source.path).bind (place policy · source) with
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
    | .ok text => pure ((parse text).mapError IntakeError.malformed |>.map (rebuild policy))
    | .error problem => pure (.error (.unreadable (sourceName source) (toString problem)))

end Linter.Intake
