private def banner : String := "first -- not a comment
dbg_trace second"
private def label (x : Option String) : String := s!"{x.getD ""}"
private def tag : Lean.Name := `sorry
private def «sorry» : Nat := 1
private def other : Nat := «sorry»
private def quote : Char := '"'
private def traced (n : Nat) : Nat := dbg_trace "n"; n
