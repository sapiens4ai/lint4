import Linter.Lexer.Common

namespace Linter.Lexer.Lean

open Linter Linter.Lexer

def isNameChar (c : Char) : Bool := c.isAlphanum || "_'!?".toList.contains c

def isSegmentStart (c : Char) : Bool := isNameStart c || c == '«'

def segment : List Char → List Char × List Char
  | '«' :: rest =>
    match rest.span (· != '»') with
    | (body, '»' :: after) => ('«' :: body ++ ['»'], after)
    | _ => ([], '«' :: rest)
  | chars => chars.span isNameChar

def name : Nat → List Char → List Char → List Char × List Char
  | fuel, taken, chars =>
    let (word, rest) := segment chars
    let taken := word.reverse ++ taken
    match fuel, rest with
    | fuel + 1, '.' :: next :: more =>
      if isSegmentStart next && !(segment (next :: more)).1.isEmpty then name fuel ('.' :: taken) (next :: more)
      else (taken.reverse, rest)
    | _, _ => (taken.reverse, rest)

def piece (interpolated : Bool) : List Char → List Char → Option (List Char × Bool × List Char)
  | acc, '\\' :: c :: rest => piece interpolated (c :: '\\' :: acc) rest
  | acc, '"' :: rest => some (('"' :: acc).reverse, false, rest)
  | acc, '{' :: rest => if interpolated then some (('{' :: acc).reverse, true, rest) else piece interpolated ('{' :: acc) rest
  | acc, c :: rest => piece interpolated (c :: acc) rest
  | _, [] => none

def interpolators : List (List Char) := ["s!", "f!", "m!"].map String.toList

def backticked : List Char → List Char × List Char
  | '`' :: inner => (['`', '`'], inner)
  | chars => (['`'], chars)

def lex : Nat → List Nat → Position → Array Token → List Char → Except LexError (Array Token)
  | 0, _, position, _, _ => .error ⟨position, "input too long"⟩
  | _, [], _, tokens, [] => .ok tokens
  | _, _ :: _, position, _, [] => .error ⟨position, "unterminated string"⟩
  | fuel + 1, frames, position, tokens, chars@(c :: rest) =>
    let emitIn (inner : List Nat) (kind : TokenKind) (taken after : List Char) :=
      lex fuel inner (advance position taken) (tokens.push (token kind position taken)) after
    let emit := emitIn frames
    let fail (message : String) : Except LexError (Array Token) := .error ⟨position, message⟩
    let string (interpolated : Bool) (outer : List Nat) (start : Position) (before : Array Token) (opening : Char) (more : List Char) :=
      match piece interpolated [opening] more with
      | some (taken, opened, after) =>
        lex fuel (if opened then 0 :: outer else outer) (advance start taken) (before.push (token .text start taken)) after
      | none => fail "unterminated string"
    match chars with
    | '/' :: '-' :: more =>
      match nested '/' '/' 0 ['-', '/'] more with
      | some (taken, after) => emit .comment taken after
      | none => fail "unterminated block comment"
    | '-' :: '-' :: _ =>
      let (taken, after) := untilNewline chars
      emit .comment taken after
    | '\'' :: '\\' :: escaped :: '\'' :: after => emit .character ['\'', '\\', escaped, '\''] after
    | '\'' :: inner :: '\'' :: after => emit .character ['\'', inner, '\''] after
    | '"' :: more => string false frames position tokens '"' more
    | '`' :: more =>
      let (ticks, body) := backticked more
      match name (fuel + 1) [] body with
      | ([], _) => emit .symbol ['`'] more
      | (taken, after) => emit .symbol (ticks ++ taken) after
    | _ =>
      if c.isWhitespace then lex fuel frames (step position c) tokens rest
      else if isSegmentStart c then
        match name (fuel + 1) [] chars with
        | ([], _) => fail "unterminated name"
        | (taken, '"' :: more) =>
          if interpolators.contains taken then
            string true frames (advance position taken) (tokens.push (token .identifier position taken)) '"' more
          else emit .identifier taken ('"' :: more)
        | (taken, after) => emit .identifier taken after
      else if c.isDigit then
        let (taken, after) := number [] chars
        emit .number taken after
      else if c == '{' then emitIn (deeper frames) .symbol [c] rest
      else if c == '}' then
        match frames with
        | 0 :: outer => string true outer position tokens '}' rest
        | (depth + 1) :: outer => emitIn (depth :: outer) .symbol [c] rest
        | [] => emit .symbol [c] rest
      else if isBracket c then emit .symbol [c] rest
      else if isSymbol c then
        let (taken, after) := chars.span isSymbol
        emit .symbol taken after
      else emit .symbol [c] rest

def tokens (source : String) : Except LexError (Array Token) :=
  let chars := source.toList
  lex (chars.length + 1) [] ⟨1, 1⟩ #[] chars

end Linter.Lexer.Lean
