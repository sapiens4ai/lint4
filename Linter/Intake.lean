import Linter.Domain
import Linter.Intake.Diff
import Linter.Intake.Archive

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
  limits : Limits

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
  | oversized (excess : Excess)
  | escaping (entry : String)
  | nothing

def IntakeError.message : IntakeError → String
  | .unpack archive detail => s!"cannot unpack {archive}: {detail}"
  | .unreadable source detail => s!"cannot read {source}: {detail}"
  | .malformed detail => detail
  | .empty => "the diff is empty: nothing to check"
  | .coloured => "the diff contains terminal colour codes: produce it with git diff --no-color --unified=100000"
  | .unchanged => "no changed file found: send a unified diff with hunks, produced by git diff --no-color --unified=100000"
  | .oversized excess => excess.message
  | .escaping entry => s!"archive entry {entry} points outside the archive"
  | .nothing => "nothing to check: the archive has no .hs, .purs, .lean or FFI .js files"

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

def admitted (limits : Limits) (found : List Entry) : Except IntakeError Unit :=
  match found, found.find? escapes, excess limits found with
  | [], _, _ => .error .nothing
  | _, some entry, _ => .error (.escaping entry.name)
  | _, none, some problem => .error (.oversized problem)
  | _ :: _, none, none => .ok ()

def isChecked : Discovered → Bool
  | .checked _ | .unreadable _ _ => true
  | .orphan _ => false

def extracted (policy : Policy) (archive dir : FilePath) : IO (Except IntakeError Submission) := do
  let out ← IO.Process.output { cmd := "unzip", args := #["-qq", archive.toString, "-d", dir.toString] }
  if out.exitCode != 0 then return .error (.unpack archive out.stderr.trimAscii.toString)
  let codebase ← collect policy (← IO.FS.realPath dir)
  return if codebase.found.any isChecked then .ok (.full codebase) else .error .nothing

def unpacked (policy : Policy) (archive : FilePath) : IO (Except IntakeError Submission) := do
  let listing ← IO.Process.output { cmd := "unzip", args := #["-Z", "-s", archive.toString] }
  if listing.exitCode > 1 then return .error (.unpack archive (listing.stderr ++ listing.stdout).trimAscii.toString)
  match admitted policy.limits (entries listing.stdout) with
  | .error problem => pure (.error problem)
  | .ok () => IO.FS.withTempDir (extracted policy archive)

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
