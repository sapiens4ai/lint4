import Linter.Lexer.Haskell
import Linter.Lexer.Lean

namespace Linter

def lexFile : Language → String → Except LexError (Array Token)
  | .haskell, source => Lexer.Haskell.tokens source
  | .purescript, source => Lexer.Haskell.tokens source
  | .lean, source => Lexer.Lean.tokens source
  | .javascript, _ => .error ⟨⟨1, 1⟩, "javascript lexer not available yet"⟩

end Linter
