import Lean
open Lean
def quoted : MacroM Syntax := `(command| #eval 1)
def a (xs : List Nat) := (xs.head!, xs.head!)
def b (xs : List Nat) := xs.tail!
def c (xs : Array (Option Nat)) := xs.findSome! id
def safe (xs : List Nat) := xs.head?
def bad : IO Unit := do
  try throw (IO.userError "failed") catch _ => pure ()
def fallback : IO Bool := do
  try throw (IO.userError "failed") catch _ => pure false
def pause := IO.sleep
def wait := pause 10
def index (xs : Array Nat) := xs[0]!
def abort : Nat := panic! "failed"
theorem unfinished : False := by sorry
theorem dependent : False := unfinished
example : False := by sorry
example : False := unfinished
example (xs : List Nat) : Nat := xs.head!
example (xs : List Nat) : Nat := helper xs
where helper (ys : List Nat) := ys.head!
example : IO Unit := do
  let rec helper := do
    IO.sleep 10
    IO.sleep 10
  helper
def traced (n : Nat) : Nat := dbg_trace "n"; n
def total! (n : Nat) : Nat := n + 1
def usesTotal (n : Nat) : Nat := total! n
def fails! (n : Nat) : Nat := if n == 0 then panic! "zero" else n
def usesFails (n : Nat) : Nat := fails! n
theorem tactic : True := by
  dbg_trace "proof"
  trivial
