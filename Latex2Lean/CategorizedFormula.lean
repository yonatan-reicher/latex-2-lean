import Latex2Lean.Formula


namespace Latex2Lean


inductive CategorizedFormula where
  | definition (name : String) (rhs : Formula)
  | axiom_ (f : Formula)
  | plain (f : Formula)
  deriving BEq, Inhabited, Repr
