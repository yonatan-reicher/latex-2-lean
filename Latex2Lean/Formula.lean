import Latex2Lean.Pos
import Latex2Lean.Range


namespace Latex2Lean


inductive BinOp where
  -- numeric symbols
  | plus
  | minus
  | star
  | slash
  -- other symbols
  | eq
  -- words
  | cap
  | cup
  | in_
  deriving DecidableEq, Inhabited, Repr


def BinOp.toString : BinOp → String
  | plus => "+"
  | minus => "-"
  | star => "*"
  | slash => "/"
  | eq => "="
  | cap => r"\cap"
  | cup => r"\cup"
  | in_ => r"\in"


instance : ToString BinOp where
  toString := BinOp.toString


/--
A formula object is an Abstact Syntax Tree of the code inside an inline-math
section of our text.
-/
inductive Formula where
  | emptySet (range : Range)
  | var (name : Array Char) (range : Range)
  | number (n : Nat) (range : Range)
  | binOp (left : Formula) (op : BinOp) (right : Formula)
  deriving DecidableEq, Repr, Inhabited

def Formula.range : Formula → Range
  | .emptySet r => r
  | .var _ r => r
  | .number _ r => r
  | .binOp l _ r => l.range ∪ r.range
