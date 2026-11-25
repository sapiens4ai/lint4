import Linter.Lexer.Haskell
import Linter.Lexer.Lean
import Linter.Lexer.JavaScript

namespace Linter

def lexFile : Language → String → Except LexError (Array Token)
  | .haskell, source => Lexer.Haskell.tokens source
  | .purescript, source => Lexer.Haskell.tokens source
  | .lean, source => Lexer.Lean.tokens source
  | .javascript, source => Lexer.JavaScript.tokens source

end Linter
