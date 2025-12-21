import Latex2Lean.Pos


namespace Latex2Lean


inductive Token.Kind
  | word (chars : Array Char)
  | number (number : Nat)
  deriving DecidableEq, Repr


structure Token where
  kind : Token.Kind
  /-- The text range of the token in the original text -/
  range : Range


/-- This is like a version of `Token.Kind.word` but that's meant to be used as a
  nicer match pattern, because it has a string argument! -/
@[match_pattern]
def Token.Kind.word' : String → Token.Kind
  | s => .word s.toList.toArray
