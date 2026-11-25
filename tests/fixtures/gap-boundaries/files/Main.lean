structure Box where
  size : Size
private def scale (n : Nat) : Size := ⟨n⟩

def main (args : List String) : IO UInt32 := pure 0
