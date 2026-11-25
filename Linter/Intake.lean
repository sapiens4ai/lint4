import Linter.Domain

namespace Linter.Intake

open Linter System

structure Policy where
  vendored : List String

inductive Input where
  | archive (path : FilePath)

def relative (root path : FilePath) : String :=
  (path.toString.drop (root.toString.length + 1)).toString

def collect (policy : Policy) (root : FilePath) : IO (List CheckedFile) := do
  let paths ← root.walkDir fun dir => do
    let link := (← dir.symlinkMetadata).type == .symlink
    pure (!link && !(dir.fileName.any policy.vendored.contains))
  let mut files : List CheckedFile := []
  for path in paths do
    if (← path.symlinkMetadata).type == .symlink then continue
    if let some language := path.extension.bind Language.ofExtension then
      files := files ++ [⟨language, ⟨relative root path, ← IO.FS.readFile path⟩⟩]
  pure files

def read (policy : Policy) : Input → IO (Except String Submission)
  | .archive archive =>
    IO.FS.withTempDir fun dir => do
      let out ← IO.Process.output { cmd := "unzip", args := #["-qq", archive.toString, "-d", dir.toString] }
      if out.exitCode != 0 then return .error s!"cannot unpack {archive}: {out.stderr.trimAscii}"
      let root ← IO.FS.realPath dir
      return .ok (.full (← collect policy root))

end Linter.Intake
