import Lean.Expr


namespace Latex2Lean


inductive LeanCmd where
  | def_ (name : Lean.Name) (e : Lean.Expr)
  | axiom_ (e : Lean.Expr)
  deriving Inhabited, Repr, BEq
