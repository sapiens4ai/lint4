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

def literal (close : Char) : List Char → List Char → Option (List Char × List Char)
  | acc, '\\' :: c :: rest => literal close (c :: '\\' :: acc) rest
  | acc, c :: rest =>
    if c == close then some ((c :: acc).reverse, rest)
    else if c == '\n' then none
    else literal close (c :: acc) rest
  | _, [] => none

inductive Ending where
  | closed
  | interpolation

def piece : List Char → List Char → Option (List Char × Ending × List Char)
  | acc, '\\' :: c :: rest => piece (c :: '\\' :: acc) rest
  | acc, '`' :: rest => some (('`' :: acc).reverse, .closed, rest)
  | acc, '$' :: '{' :: rest => some (('{' :: '$' :: acc).reverse, .interpolation, rest)
  | acc, c :: rest => piece (c :: acc) rest
  | _, [] => none

def deeper : List Nat → List Nat
  | depth :: outer => (depth + 1) :: outer
  | [] => []

def regex : Bool → List Char → List Char → Option (List Char × List Char)
  | _, _, '\n' :: _ => none
  | inClass, acc, '\\' :: c :: rest => if c == '\n' then none else regex inClass (c :: '\\' :: acc) rest
  | false, acc, '/' :: rest =>
    let (flags, after) := rest.span isNameChar
    some (('/' :: acc).reverse ++ flags, after)
  | _, acc, '[' :: rest => regex true ('[' :: acc) rest
  | _, acc, ']' :: rest => regex false (']' :: acc) rest
  | inClass, acc, c :: rest => regex inClass (c :: acc) rest
  | _, _, [] => none

def valueExpected (tokens : Array Token) : Bool :=
  match tokens.findRev? (·.kind != .comment) with
  | none => true
  | some last =>
    match last.kind with
    | .symbol => last.text == "=>" || last.text.toList.getLast?.any ("(,=:[!&|?{};".toList.contains ·)
    | .identifier => last.text == "return"
    | .text => last.text.endsWith "${"
    | .number | .character | .comment | .pragma => false

def operator : List Char → List Char × List Char
  | '/' :: '/' :: rest => ([], '/' :: '/' :: rest)
  | '/' :: '*' :: rest => ([], '/' :: '*' :: rest)
  | c :: rest =>
    if isSymbol c then
      let (taken, after) := operator rest
      (c :: taken, after)
    else ([], c :: rest)
  | [] => ([], [])

def lex : Nat → List Nat → Position → Array Token → List Char → Except LexError (Array Token)
  | 0, _, position, _, _ => .error ⟨position, "input too long"⟩
  | _, [], _, tokens, [] => .ok tokens
  | _, _ :: _, position, _, [] => .error ⟨position, "unterminated template literal"⟩
  | fuel + 1, frames, position, tokens, chars@(c :: rest) =>
    let emitIn (inner : List Nat) (kind : TokenKind) (taken after : List Char) :=
      lex fuel inner (advance position taken) (tokens.push (token kind position taken)) after
    let emit := emitIn frames
    let fail (message : String) : Except LexError (Array Token) := .error ⟨position, message⟩
    let text (close : Char) :=
      match literal close [close] rest with
      | some (taken, after) => emit .text taken after
      | none => fail "unterminated string"
    let template (outer : List Nat) (opening : Char) :=
      match piece [opening] rest with
      | some (taken, .closed, after) => emitIn outer .text taken after
      | some (taken, .interpolation, after) => emitIn (0 :: outer) .text taken after
      | none => fail "unterminated template literal"
    let symbolic (_ : Unit) :=
      match operator chars with
      | ([], _) => emit .symbol [c] rest
      | (taken, after) => emit .symbol taken after
    match chars with
    | '/' :: '*' :: more =>
      match block ['*', '/'] more with
      | some (taken, after) => emit .comment taken after
      | none => fail "unterminated block comment"
    | '/' :: '/' :: _ =>
      let (taken, after) := untilNewline chars
      emit .comment taken after
    | '/' :: more =>
      if valueExpected tokens then
        match regex false ['/'] more with
        | some (taken, after) => emit .text taken after
        | none => fail "unterminated regular expression"
      else symbolic ()
    | '"' :: _ => text '"'
    | '\'' :: _ => text '\''
    | '`' :: _ => template frames '`'
    | '{' :: _ => emitIn (deeper frames) .symbol [c] rest
    | '}' :: _ =>
      match frames with
      | 0 :: outer => template outer '}'
      | (depth + 1) :: outer => emitIn (depth :: outer) .symbol [c] rest
      | [] => emit .symbol [c] rest
    | _ =>
      if c.isWhitespace then lex fuel frames (step position c) tokens rest
      else if isNameStart c then
        let (taken, after) := chars.span isNameChar
        emit .identifier taken after
      else if c.isDigit then
        let (taken, after) := number chars
        emit .number taken after
      else if isBracket c then emit .symbol [c] rest
      else if isSymbol c then symbolic ()
      else emit .symbol [c] rest

def tokens (source : String) : Except LexError (Array Token) :=
  let chars := source.toList
  lex (chars.length + 1) [] ⟨1, 1⟩ #[] chars

end Linter.Lexer.JavaScript
