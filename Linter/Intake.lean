import Linter.Domain

namespace Linter.Intake

open Linter System

structure Policy where
  vendored : List String

inductive Input where
  | archive (path : FilePath)
  | diff (text : String)

def relative (root path : FilePath) : String :=
  (path.toString.drop (root.toString.length + 1)).toString

def languageOf (paths : List String) (path : String) : Option Language :=
  let file := System.FilePath.mk path
  match file.extension with
  | some "js" => if paths.contains (file.withExtension "purs").toString then some .javascript else none
  | some other => Language.ofExtension other
  | none => none

def checkedFiles (files : List SourceFile) : List CheckedFile :=
  let paths := files.map (·.path)
  files.filterMap fun file => (languageOf paths file.path).map (⟨·, file⟩)

def collect (policy : Policy) (root : FilePath) : IO (List CheckedFile) := do
  let paths ← root.walkDir fun dir => do
    let link := (← dir.symlinkMetadata).type == .symlink
    pure (!link && !(dir.fileName.any policy.vendored.contains))
  let mut plain : List FilePath := []
  for path in paths do
    if (← path.symlinkMetadata).type != .symlink then plain := path :: plain
  let names := plain.reverse.map (relative root ·)
  let mut files : List CheckedFile := []
  for path in plain.reverse do
    if let some language := languageOf names (relative root path) then
      files := ⟨language, ⟨relative root path, ← IO.FS.readFile path⟩⟩ :: files
  pure files.reverse

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
  let files := sources.filterMap fun (source, _) => (languageOf present source.path).map (⟨·, source⟩)
  .diff files (sources.map fun (source, lines) => AddedLines.mk source.path lines)

def read (policy : Policy) : Input → IO (Except String Submission)
  | .archive archive =>
    IO.FS.withTempDir fun dir => do
      let out ← IO.Process.output { cmd := "unzip", args := #["-qq", archive.toString, "-d", dir.toString] }
      if out.exitCode != 0 then return .error s!"cannot unpack {archive}: {out.stderr.trimAscii}"
      let root ← IO.FS.realPath dir
      return .ok (.full (← collect policy root))
  | .diff text => pure ((parse text).map (rebuild policy))

end Linter.Intake
