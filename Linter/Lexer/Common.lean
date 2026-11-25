import Linter.Domain

namespace Linter.Lexer

def step (position : Position) (c : Char) : Position :=
  if c == '\n' then ⟨position.line + 1, 1⟩ else ⟨position.line, position.column + 1⟩

def advance (position : Position) (chars : List Char) : Position :=
  chars.foldl step position

def token (kind : TokenKind) (start : Position) (taken : List Char) : Token :=
  ⟨kind, String.ofList taken, start, advance start taken.dropLast⟩

def isBracket (c : Char) : Bool := "(),;[]`{}".toList.contains c

def number : List Char → List Char × List Char
  | '.' :: '.' :: rest => ([], '.' :: '.' :: rest)
  | c :: rest =>
    if c.isAlphanum || c == '.' || c == '_' then
      let (taken, after) := number rest
      (c :: taken, after)
    else ([], c :: rest)
  | [] => ([], [])

def untilNewline (chars : List Char) : List Char × List Char :=
  chars.span (· != '\n')

end Linter.Lexer
