import Linter.Domain

namespace Linter.Lexer

def step (position : Position) (c : Char) : Position :=
  if c == '\n' then ⟨position.line + 1, 1⟩ else ⟨position.line, position.column + 1⟩

def advance (position : Position) (chars : List Char) : Position :=
  chars.foldl step position

def token (kind : TokenKind) (start : Position) (taken : List Char) : Token :=
  ⟨kind, String.ofList taken, start, advance start taken.dropLast⟩

def isBracket (c : Char) : Bool := "(),;[]`{}".toList.contains c

def quoted : List Char → List Char → Option (List Char × List Char)
  | acc, '\\' :: c :: rest => quoted (c :: '\\' :: acc) rest
  | acc, '"' :: rest => some (('"' :: acc).reverse, rest)
  | _, '\n' :: _ => none
  | acc, c :: rest => quoted (c :: acc) rest
  | _, [] => none

def untilNewline (chars : List Char) : List Char × List Char :=
  chars.span (· != '\n')

end Linter.Lexer
