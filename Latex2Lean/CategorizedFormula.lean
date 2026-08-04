import Latex2Lean.Formula


namespace Latex2Lean


inductive CategorizedFormula where
  /-- $x = 1 + y$ -/
  | definition (name : Array Char) (nameRange : Range) (rhs : Formula) (varId opId : Formula.Id)
  | axiom_ (f : Formula)
  | plain (f : Formula)
  deriving BEq, Inhabited, Repr


def CategorizedFormula.toFormula : CategorizedFormula → Formula
  | .definition name nameRange rhs varId opId =>
    Formula.mk opId <| .binOp (Formula.mk varId <| .var name nameRange) BinOp.eq rhs
  | .axiom_ f => f
  | .plain f => f
