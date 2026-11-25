import Linter.Lexer.Common

namespace Linter.Lexer.JavaScript

open Linter Linter.Lexer

def isSymbol (c : Char) : Bool := "!%&*+-/<=>?^|~:.@#".toList.contains c

def isNameStart (c : Char) : Bool := c.isAlpha || c == '_' || c == '$'

def isNameChar (c : Char) : Bool := c.isAlphanum || c == '_' || c == '$'

def block : List Char → List Char → Option (List Char × List Char)
  | acc, '*' :: '/' :: rest => some (('/' :: '*' :: acc).reverse, rest)
  | acc, c :: rest => block (c :: acc) rest
  | _, [] => none

def literal (close : Char) (multiline : Bool) : List Char → List Char → Option (List Char × List Char)
  | acc, '\\' :: c :: rest => literal close multiline (c :: '\\' :: acc) rest
  | acc, c :: rest =>
    if c == close then some ((c :: acc).reverse, rest)
    else if c == '\n' && !multiline then none
    else literal close multiline (c :: acc) rest
  | _, [] => none

def operator : List Char → List Char × List Char
  | '/' :: '/' :: rest => ([], '/' :: '/' :: rest)
  | '/' :: '*' :: rest => ([], '/' :: '*' :: rest)
  | c :: rest =>
    if isSymbol c then
      let (taken, after) := operator rest
      (c :: taken, after)
    else ([], c :: rest)
  | [] => ([], [])

def lex : Nat → Position → Array Token → List Char → Except LexError (Array Token)
  | 0, position, _, _ => .error ⟨position, "input too long"⟩
  | _, _, tokens, [] => .ok tokens
  | fuel + 1, position, tokens, chars@(c :: rest) =>
    let emit (kind : TokenKind) (taken after : List Char) :=
      lex fuel (advance position taken) (tokens.push (token kind position taken)) after
    let fail (message : String) : Except LexError (Array Token) := .error ⟨position, message⟩
    let text (close : Char) (multiline : Bool) :=
      match literal close multiline [close] rest with
      | some (taken, after) => emit .text taken after
      | none => fail "unterminated string"
    match chars with
    | '/' :: '*' :: more =>
      match block ['*', '/'] more with
      | some (taken, after) => emit .comment taken after
      | none => fail "unterminated block comment"
    | '/' :: '/' :: _ =>
      let (taken, after) := untilNewline chars
      emit .comment taken after
    | '"' :: _ => text '"' false
    | '\'' :: _ => text '\'' false
    | '`' :: _ => text '`' true
    | _ =>
      if c.isWhitespace then lex fuel (step position c) tokens rest
      else if isNameStart c then
        let (taken, after) := chars.span isNameChar
        emit .identifier taken after
      else if c.isDigit then
        let (taken, after) := chars.span fun d => d.isAlphanum || d == '.' || d == '_'
        emit .number taken after
      else if isBracket c then emit .symbol [c] rest
      else if isSymbol c then
        match operator chars with
        | ([], _) => emit .symbol [c] rest
        | (taken, after) => emit .symbol taken after
      else emit .symbol [c] rest

def tokens (source : String) : Except LexError (Array Token) :=
  let chars := source.toList
  lex (chars.length + 1) ⟨1, 1⟩ #[] chars

end Linter.Lexer.JavaScript
