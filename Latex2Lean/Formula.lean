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


mutual

/--
A formula object is an Abstact Syntax Tree of the code inside an inline-math
section of our text.
-/
inductive Formula where
  | emptySet (range : Range) -- TODO: Remove
  | var (name : Array Char) (range : Range)
  | number (n : Nat) (range : Range)
  | abs (inner : Formula) (range : Range)
  | binOp (left : Formula) (op : BinOp) (right : Formula)
  | simpleSet (elements : Array Formula) (range : Range)
  | mapSet (lhs : Formula) (binders : Array Formula.Binder) (range : Range)
  | tuple (elements : Array Formula) (range : Range)
  deriving Inhabited, BEq, Repr

inductive Formula.Binder where
  | in_ (name : Array Char) (set : Formula)
  deriving Inhabited, BEq, Repr

end

mutual

partial def Formula.WF : Formula → Bool
  | .emptySet _
  | .var _ _
  | .number _ _
     => true
  | .abs inner _ => inner.WF
  | .binOp l _ r => l.WF ∧ r.WF
  | .simpleSet elements _ => elements.all WF
  | .mapSet lhs binders _ => lhs.WF ∧ binders.all Binder.WF
  | .tuple elements _ => elements.size > 1 ∧ elements.all WF

partial def Formula.Binder.WF : Formula.Binder → Bool
  | .in_ _ inner => inner.WF

end

def Formula.range : Formula → Range
  | .emptySet r => r
  | .var _ r => r
  | .number _ r => r
  | .abs _ r => r
  | .binOp l _ r => l.range ∪ r.range
  | .simpleSet _ r => r
  | .mapSet _ _ r => r
  | .tuple _ r => r
