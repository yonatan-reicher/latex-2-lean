import Latex2Lean.Pos
import Latex2Lean.Range


namespace Latex2Lean


inductive Token.Kind
  | word (chars : Array Char)
  /-- A word, but starts with a '\' -/
  | command (chars : Array Char)
  | number (number : Nat)
  deriving DecidableEq, Repr, Inhabited


structure Token where
  kind : Token.Kind
  /-- The text range of the token in the original text -/
  range : Range
  deriving Inhabited


/-- This is like a version of `Token.Kind.word` but that's meant to be used as a
  nicer match pattern, because it has a string argument! -/
@[match_pattern]
def Token.Kind.word' : String → Token.Kind
  | s => .word s.toList.toArray


/-- This is like a version of `Token.Kind.command` but that's meant to be used as a
  nicer match pattern, because it has a string argument! -/
@[match_pattern]
def Token.Kind.command' : String → Token.Kind
  | s => .command s.toList.toArray
