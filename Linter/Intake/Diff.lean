namespace Linter.Intake

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
  let excess := s!"{label changed}: hunk has more lines than its header counts: {line}"
  match line.toList, state.oldLeft, state.newLeft with
  | ' ' :: _, old + 1, new + 1 => .ok { state with current := kept changed text false, oldLeft := old, newLeft := new }
  | [], old + 1, new + 1 => .ok { state with current := kept changed "" false, oldLeft := old, newLeft := new }
  | '+' :: _, _, new + 1 => .ok { state with current := kept changed text true, newLeft := new }
  | '-' :: _, old + 1, _ => .ok { state with oldLeft := old }
  | '\\' :: _, _, _ => .ok state
  | ' ' :: _, _, _ | [], _, _ | '+' :: _, _, _ | '-' :: _, _, _ => .error excess
  | _, _, _ => .error s!"{label changed}: malformed hunk line: {line}"

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

def diffLines (text : String) : List String :=
  match (text.splitOn "\n").reverse with
  | "" :: lines => lines.reverse
  | lines => lines.reverse

def parse (text : String) : Except String (List Changed) := do
  let state ← (diffLines text).foldlM step ⟨#[], none, fresh, 0, 0⟩
  if state.oldLeft + state.newLeft > 0 then throw "diff ends inside a hunk: the hunk has fewer lines than its header counts"
  let changes := (closed state).done.toList
  match repeatedPath changes with
  | some path => throw s!"malformed diff: {path} appears more than once"
  | none => pure changes

end Linter.Intake
