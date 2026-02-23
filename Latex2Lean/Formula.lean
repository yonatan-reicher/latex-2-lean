import Latex2Lean.Util
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


mutual

partial def Formula.toString : Formula → String
  | .emptySet _ => "\\emptyset"
  | .var name _ => s!"{show String from name}"
  | .number n _ => s!"{n}"
  | .abs inner _ => s!"\\abs {inner.toString}"
  | .binOp left op right =>
    s!"{left.toString} {op} {right.toString}"
  | .simpleSet elements _ =>
    ", ".intercalate (elements.toList.map Formula.toString)
    |> (s!"\\\{ {·} \\}")
  | .mapSet lhs binders _ =>
    s!"\\\{ {lhs.toString} \\mid {", ".intercalate (binders.toList.map Formula.Binder.toString)} \\}"
  | .tuple elements _ =>
    s!"({", ".intercalate (elements.toList.map Formula.toString)})"

partial def Formula.Binder.toString : Formula.Binder → String
  | .in_ name set => s!"{show String from name} \\in {set.toString}"

end

instance : ToString Formula := ⟨Formula.toString⟩
instance : ToString Formula.Binder := ⟨Formula.Binder.toString⟩

#guard
  Formula.mapSet
    (.binOp (.var "x" default) .plus (.number 1 default))
    #[.in_ "x" $ .emptySet default
    , .in_ "y" $ .simpleSet #[.number 5 default] default]
    default
  |>.toString
  |> (· == r"\{ x + 1 \mid x \in \emptyset, y \in \{ 5 \} \}")
