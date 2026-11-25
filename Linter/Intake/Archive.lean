namespace Linter.Intake

structure Limits where
  entries : Nat
  total : Nat
  entry : Nat

structure Entry where
  name : String
  size : Nat

inductive Excess where
  | entries (count limit : Nat)
  | total (bytes limit : Nat)
  | entry (name : String) (bytes limit : Nat)

def Excess.message : Excess → String
  | .entries count limit => s!"the archive has {count} entries; the limit is {limit}"
  | .total bytes limit => s!"the archive unpacks to {bytes} bytes; the limit is {limit}"
  | .entry name bytes limit => s!"archive entry {name} unpacks to {bytes} bytes; the limit is {limit}"

def afterFields : Nat → List Char → List Char
  | 0, chars => chars
  | count + 1, chars => afterFields count ((chars.dropWhile (· == ' ')).dropWhile (· != ' '))

def listed (line : String) : Option Entry :=
  match (line.splitOn " ").filter (· != "") with
  | mode :: _ :: _ :: size :: _ :: _ :: _ :: _ :: _ :: _ =>
    if mode.length == 10 then size.toNat?.map (Entry.mk (String.ofList ((afterFields 8 line.toList).drop 1)) ·) else none
  | _ => none

def entries (listing : String) : List Entry :=
  (listing.splitOn "\n").filterMap listed

def escapes (entry : Entry) : Bool :=
  let path := entry.name.map fun c => if c == '\\' then '/' else c
  path.startsWith "/" || (path.splitOn "/").contains ".."

def excess (limits : Limits) (found : List Entry) : Option Excess :=
  let total := found.foldl (· + ·.size) 0
  if found.length > limits.entries then some (.entries found.length limits.entries)
  else if total > limits.total then some (.total total limits.total)
  else (found.find? (·.size > limits.entry)).map fun entry => .entry entry.name entry.size limits.entry

end Linter.Intake
