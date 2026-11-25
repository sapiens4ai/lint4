namespace Linter

inductive Severity where
  | noGo | warning | couldBeBetter
  deriving Repr, BEq

def Severity.word : Severity → String
  | .noGo => "no-go"
  | .warning => "warning"
  | .couldBeBetter => "could-be-better"

inductive Language where
  | haskell | purescript | javascript | lean
  deriving Repr, BEq

def Language.word : Language → String
  | .haskell => "haskell"
  | .purescript => "purescript"
  | .javascript => "javascript"
  | .lean => "lean"

def Language.all : List Language := [.haskell, .purescript, .javascript, .lean]

def Language.ofExtension : String → Option Language
  | "hs" => some .haskell
  | "purs" => some .purescript
  | "lean" => some .lean
  | _ => none

inductive Category where
  | safety | style | types | errors | boundary | security | checks
  deriving Repr, BEq

def Category.word : Category → String
  | .safety => "safety"
  | .style => "style"
  | .types => "types"
  | .errors => "errors"
  | .boundary => "boundary"
  | .security => "security"
  | .checks => "checks"

structure RuleId where
  category : Category
  name : String
  deriving Repr, BEq

def RuleId.word (id : RuleId) : String := s!"{id.category.word}/{id.name}"

inductive TokenKind where
  | identifier | symbol | number | text | character | comment | pragma
  deriving Repr, BEq

structure Position where
  line : Nat
  column : Nat
  deriving Repr, BEq

structure Token where
  kind : TokenKind
  text : String
  start : Position
  stop : Position
  deriving Repr

structure LexError where
  position : Position
  message : String

inductive TokenShape where
  | kind (kind : TokenKind)
  | word (kind : TokenKind) (text : String)
  | segment (name : String)
  | containing (kind : TokenKind) (fragment : String)
  | suffix (ending : String)
  | except (shape : TokenShape) (excluded : TokenShape)

inductive Piece where
  | exact (shape : TokenShape)
  | anyOf (shapes : List TokenShape)
  | lineStart (shape : TokenShape)
  | many (shape : TokenShape)
  | notBefore (shapes : List TokenShape)
  | gap (limit : Nat)
  | skipBalanced

inductive Region where
  | hidden
  | anywhere
  | outside (header : TokenShape)

def Region.code : Region := .outside (.word .identifier "import")

structure Tier where
  above : Nat
  severity : Severity

inductive Matcher where
  | tokens (languages : List Language) (region : Region) (pieces : List Piece)
  | lineDensity (languages : List Language) (shapes : List TokenShape) (atLeast : Nat)
  | declarationTokens (languages : List Language) (limit : Nat)
  | importShare (languages : List Language) (header : TokenShape) (percent : Nat)
  | repeatedLiteral (languages : List Language) (minModules : Nat) (minLength : Nat) (ignored : List String)
  | duplicateBody (languages : List Language) (minTokens : Nat) (ignored : List String)
  | ffiPairing (foreign : List Piece) (exempt : TokenShape)

inductive Detector where
  | fixed (severity : Severity) (matcher : Matcher)
  | lineLength (languages : List Language) (tiers : List Tier)

structure SourceFile where
  path : String
  code : String

structure Example where
  bad : List SourceFile
  good : List SourceFile

structure Check where
  detector : Detector
  examples : List Example
  hasExamples : examples ≠ [] := by simp

structure Rule where
  id : RuleId
  why : String
  fix : String
  checks : List Check
  hasChecks : checks ≠ [] := by simp

structure CheckedFile where
  language : Language
  source : SourceFile

structure AddedLines where
  path : String
  lines : List Nat

structure FileError where
  path : String
  message : String

inductive Discovered where
  | checked (file : CheckedFile)
  | unreadable (language : Language) (problem : FileError)
  | orphan (path : String)

structure Codebase where
  found : List Discovered
  paired : List String

inductive Submission where
  | full (codebase : Codebase)
  | diff (files : List CheckedFile) (added : List AddedLines)

inductive Mode where
  | full | diff

def Mode.word : Mode → String
  | .full => "full"
  | .diff => "diff"

structure Span where
  first : Nat
  last : Nat

structure Finding where
  rule : RuleId
  severity : Severity
  path : String
  line : Nat
  column : Nat
  span : Span
  snippet : String
  why : String
  fix : String

inductive Outcome where
  | passed | blocked | incomplete
  deriving BEq

def Outcome.word : Outcome → String
  | .passed => "passed"
  | .blocked => "blocked"
  | .incomplete => "incomplete"

def Outcome.exitCode : Outcome → UInt32
  | .passed => 0
  | .blocked => 1
  | .incomplete => 2

structure Report where
  mode : Mode
  outcome : Outcome
  checked : List (Language × Nat)
  findings : List Finding
  errors : List FileError

end Linter
