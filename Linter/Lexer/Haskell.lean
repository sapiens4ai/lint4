import Linter.Lexer.Common

namespace Linter.Lexer.Haskell

open Linter Linter.Lexer

def isSymbol (c : Char) : Bool := "!#$%&*+./<=>?@\\^|-~:".toList.contains c

def isNameStart (c : Char) : Bool := c.isAlpha || c == '_'

def isNameChar (c : Char) : Bool := c.isAlphanum || c == '_' || c == '\''

def nested : Nat → List Char → List Char → Option (List Char × List Char)
  | depth, acc, '{' :: '-' :: rest => nested (depth + 1) ('-' :: '{' :: acc) rest
  | 0, acc, '-' :: '}' :: rest => some (('}' :: '-' :: acc).reverse, rest)
  | depth + 1, acc, '-' :: '}' :: rest => nested depth ('}' :: '-' :: acc) rest
  | depth, acc, c :: rest => nested depth (c :: acc) rest
  | _, _, [] => none

def pragma : List Char → List Char → Option (List Char × List Char)
  | acc, '#' :: '-' :: '}' :: rest => some (('}' :: '-' :: '#' :: acc).reverse, rest)
  | acc, c :: rest => pragma (c :: acc) rest
  | _, [] => none

def string : Bool → List Char → List Char → Option (List Char × List Char)
  | true, acc, '\\' :: rest => string false ('\\' :: acc) rest
  | true, acc, c :: rest => if c.isWhitespace then string true (c :: acc) rest else none
  | false, acc, '\\' :: c :: rest => string c.isWhitespace (c :: '\\' :: acc) rest
  | false, acc, '"' :: rest => some (('"' :: acc).reverse, rest)
  | false, _, '\n' :: _ => none
  | false, acc, c :: rest => string false (c :: acc) rest
  | _, _, [] => none

def character : List Char → Option (List Char × List Char)
  | '\\' :: c :: rest =>
    let (body, tail) := rest.span (· != '\'')
    match tail with
    | '\'' :: after => some ('\'' :: '\\' :: c :: body ++ ['\''], after)
    | _ => none
  | c :: '\'' :: rest => some (['\'', c, '\''], rest)
  | _ => none

def name : Nat → List Char → List Char × List Char
  | 0, chars => ([], chars)
  | fuel + 1, chars =>
    let (word, rest) := chars.span isNameChar
    match word.head?, rest with
    | some first, '.' :: next :: more =>
      if first.isUpper && isNameStart next then
        let (tail, after) := name fuel (next :: more)
        (word ++ '.' :: tail, after)
      else (word, rest)
    | _, _ => (word, rest)

def lex : Nat → Position → Array Token → List Char → Except LexError (Array Token)
  | 0, position, _, _ => .error ⟨position, "input too long"⟩
  | _, _, tokens, [] => .ok tokens
  | fuel + 1, position, tokens, chars@(c :: rest) =>
    let emit (kind : TokenKind) (taken after : List Char) :=
      lex fuel (advance position taken) (tokens.push (token kind position taken)) after
    let fail (message : String) : Except LexError (Array Token) := .error ⟨position, message⟩
    match chars with
    | '{' :: '-' :: '#' :: more =>
      match pragma ['#', '-', '{'] more with
      | some (taken, after) => emit .pragma taken after
      | none => fail "unterminated pragma"
    | '{' :: '-' :: more =>
      match nested 0 ['-', '{'] more with
      | some (taken, after) => emit .comment taken after
      | none => fail "unterminated block comment"
    | '-' :: '-' :: _ =>
      let (dashes, after) := chars.span (· == '-')
      match after with
      | next :: _ =>
        if isSymbol next then
          let (operator, tail) := after.span isSymbol
          emit .symbol (dashes ++ operator) tail
        else
          let (body, tail) := untilNewline after
          emit .comment (dashes ++ body) tail
      | [] => emit .comment dashes []
    | '"' :: more =>
      match string false ['"'] more with
      | some (taken, after) => emit .text taken after
      | none => fail "unterminated string"
    | '\'' :: more =>
      match character more with
      | some (taken, after) => emit .character taken after
      | none => emit .symbol ['\''] more
    | _ =>
      if c.isWhitespace then lex fuel (step position c) tokens rest
      else if isNameStart c then
        let (taken, after) := name (fuel + 1) chars
        emit .identifier taken after
      else if c.isDigit then
        let (taken, after) := number chars
        emit .number taken after
      else if isBracket c then emit .symbol [c] rest
      else if isSymbol c then
        let (taken, after) := chars.span isSymbol
        emit .symbol taken after
      else emit .symbol [c] rest

def tokens (source : String) : Except LexError (Array Token) :=
  let chars := source.toList
  lex (chars.length + 1) ⟨1, 1⟩ #[] chars

end Linter.Lexer.Haskell
