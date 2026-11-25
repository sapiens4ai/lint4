import Linter.Domain

namespace Linter.Lexer

def step (position : Position) (c : Char) : Position :=
  if c == '\n' then ⟨position.line + 1, 1⟩ else ⟨position.line, position.column + 1⟩

def advance (position : Position) (chars : List Char) : Position :=
  chars.foldl step position

def token (kind : TokenKind) (start : Position) (taken : List Char) : Token :=
  ⟨kind, String.ofList taken, start, advance start taken.dropLast⟩

def isSymbol (c : Char) : Bool := "!#$%&*+./<=>?@\\^|-~:".toList.contains c

def isNameStart (c : Char) : Bool := c.isAlpha || c == '_'

def isBracket (c : Char) : Bool := "(),;[]`{}".toList.contains c

def number : List Char → List Char → List Char × List Char
  | taken, '.' :: '.' :: rest => (taken.reverse, '.' :: '.' :: rest)
  | taken, c :: rest => if c.isAlphanum || c == '.' || c == '_' then number (c :: taken) rest else (taken.reverse, c :: rest)
  | taken, [] => (taken.reverse, [])

def untilNewline (chars : List Char) : List Char × List Char :=
  chars.span (· != '\n')

def nested (opening closing : Char) : Nat → List Char → List Char → Option (List Char × List Char)
  | depth, acc, c :: tail@(next :: rest) =>
    if c == opening && next == '-' then nested opening closing (depth + 1) ('-' :: c :: acc) rest
    else if c == '-' && next == closing then
      match depth with
      | 0 => some ((next :: '-' :: acc).reverse, rest)
      | outer + 1 => nested opening closing outer (next :: '-' :: acc) rest
    else nested opening closing depth (c :: acc) tail
  | _, _, _ => none

def deeper : List Nat → List Nat
  | depth :: outer => (depth + 1) :: outer
  | [] => []

end Linter.Lexer
