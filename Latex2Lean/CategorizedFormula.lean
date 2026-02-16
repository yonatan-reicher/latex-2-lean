import Latex2Lean.Formula


namespace Latex2Lean


inductive CategorizedFormula where
  | definition (name : Array Char) (nameRange : Range) (rhs : Formula)
  | axiom_ (f : Formula)
  | plain (f : Formula)
  deriving BEq, Inhabited, Repr


def CategorizedFormula.toFormula : CategorizedFormula → Formula
  | .definition name nameRange rhs =>
    Formula.binOp (Formula.var name nameRange) BinOp.eq rhs
  | .axiom_ f => f
  | .plain f => f
