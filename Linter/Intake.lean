import Linter.Domain

namespace Linter.Intake

open Linter System

structure Policy where
  vendored : List String

inductive Input where
  | archive (path : FilePath)

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

def read (policy : Policy) : Input → IO (Except String Submission)
  | .archive archive =>
    IO.FS.withTempDir fun dir => do
      let out ← IO.Process.output { cmd := "unzip", args := #["-qq", archive.toString, "-d", dir.toString] }
      if out.exitCode != 0 then return .error s!"cannot unpack {archive}: {out.stderr.trimAscii}"
      let root ← IO.FS.realPath dir
      return .ok (.full (← collect policy root))

end Linter.Intake
