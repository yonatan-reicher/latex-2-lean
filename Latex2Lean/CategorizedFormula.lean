import Latex2Lean.Formula


namespace Latex2Lean

private abbrev FId := Formula.Id


inductive CategorizedFormula where
  /-- $x = 1 + y$ -/
  | definition (id : FId) (name : Array Char) (nameRange : Range) (rhs : Formula) (varId opId : FId)
  | axiom_ (id : FId) (f : Formula)
  | plain (id : FId) (f : Formula)
  deriving BEq, Inhabited, Repr

def CategorizedFormula.toFormula : CategorizedFormula → Formula
  | .definition _ name nameRange rhs varId opId =>
    Formula.mk opId <| .binOp (Formula.mk varId <| .var name nameRange) BinOp.eq rhs
  | .axiom_ _ f => f
  | .plain _ f => f
