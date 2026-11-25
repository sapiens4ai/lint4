import Linter.Domain

namespace Linter.Catalogue.Metrics

open Linter

def repeatedChar (count : Nat) (c : Char) : String := String.ofList (List.replicate count c)

def longLine : Rule where
  id := ⟨.style, "long-line"⟩
  why := "A long line hides logic past the edge of the screen and in review diffs"
  fix := "Split the line into named steps or wrap it"
  checks := [
    { detector := .lineLength [.haskell, .purescript, .javascript, .lean] [⟨160, .warning⟩, ⟨250, .noGo⟩]
      examples := [
        { bad := [⟨"Bad.hs", s!"name = \"{repeatedChar 170 'a'}\"\n"⟩],
          good := [⟨"Good.hs", s!"name = \"{repeatedChar 100 'a'}\"\n"⟩] },
        { bad := [⟨"Bad.lean", s!"def name := \"{repeatedChar 300 'a'}\"\n"⟩],
          good := [⟨"Good.lean", "def name := \"short\"\n"⟩] }] }]

def denseLine : Rule where
  id := ⟨.style, "dense-line"⟩
  why := "Several conditions or statements on one line are hard to read and to review"
  fix := "Give each condition a name or put each statement on its own line"
  checks := [
    { detector := .fixed .warning (.lineDensity [.haskell, .purescript, .javascript, .lean]
        [.word .identifier "if", .word .symbol "&&", .word .symbol "||", .word .symbol ";"] 3)
      examples := [{ bad := [⟨"Bad.hs", "pick a b c = if a && b || c then 1 else 0\n"⟩],
                     good := [⟨"Good.hs", "pick a b = if a && b then 1 else 0\nlabel = \"if && || ;\"\n"⟩] }] }]

def largeDeclaration : Rule where
  id := ⟨.style, "large-declaration"⟩
  why := "A very large declaration does too many things to understand or test in one piece"
  fix := "Split the declaration into smaller named parts"
  checks := [
    { detector := .fixed .couldBeBetter (.declarationTokens Language.all 300)
      examples := [{ bad := [⟨"Bad.hs", "total = " ++ " + ".intercalate (List.replicate 160 "1") ++ "\n"⟩],
                     good := [⟨"Good.hs", "total = " ++ " + ".intercalate (List.replicate 20 "1") ++ "\n"⟩] }] }]

def importShare : Rule where
  id := ⟨.style, "import-share"⟩
  why := "A module that is mostly imports glues too many concerns together"
  fix := "Move the code into the modules it depends on or split the module"
  checks := [
    { detector := .fixed .couldBeBetter (.importShare [.purescript] (.word .identifier "import") 10)
      examples := [{ bad := [⟨"Bad.purs", "module Bad where\nimport Prelude\nimport Data.Maybe\nx = 1\n"⟩],
                     good := [⟨"Good.purs", "module Good where\nimport Prelude\n" ++ "\n".intercalate
                       ((List.range 12).map fun index => s!"x{index} = {index}") ++ "\n"⟩] }] }]

def constant : Rule where
  id := ⟨.style, "constant"⟩
  why := "The same magic number in many modules drifts apart when one copy changes"
  fix := "Define the number once as a named constant and import it"
  checks := [
    { detector := .fixed .couldBeBetter (.repeatedLiteral [.haskell, .purescript] 3 2 ["0.0", "1.0"])
      examples := [{ bad := [⟨"A.hs", "limit = 4096\n"⟩, ⟨"B.hs", "size = 4096\n"⟩, ⟨"C.purs", "cap = 4096\n"⟩],
                     good := [⟨"A.hs", "limit = 4096\nratio = 1.0\n"⟩, ⟨"B.hs", "size = 4096\nratio = 1.0\n"⟩,
                              ⟨"C.hs", "cap = 1\nratio = 1.0\n"⟩] }] }]

def duplicate : Rule where
  id := ⟨.style, "duplicate"⟩
  why := "A copied declaration has to be fixed in every copy"
  fix := "Keep one definition in a shared module and import it"
  checks := [
    { detector := .fixed .couldBeBetter (.duplicateBody [.haskell, .purescript] 12 ["main", "import", "module"])
      examples := [{ bad := [⟨"A.hs", "double xs = map (\\x -> x * 2 + 1) (filter even xs)\n"⟩,
                             ⟨"B.hs", "twice xs = map (\\x -> x * 2 + 1) (filter even xs)\n"⟩],
                     good := [⟨"A.hs", "double xs = map (\\x -> x * 2 + 1) (filter even xs)\nmain = print (double [1, 2, 3, 4])\n"⟩,
                              ⟨"B.hs", "triple xs = map (\\x -> x * 3 + 1) (filter odd xs)\nmain = print (double [1, 2, 3, 4])\n"⟩] }] }]

def rules : List Rule := [longLine, denseLine, largeDeclaration, importShare, constant, duplicate]

end Linter.Catalogue.Metrics
