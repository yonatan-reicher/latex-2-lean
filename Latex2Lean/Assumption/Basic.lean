import Latex2Lean.Node.Basic


structure Assumption where
  expr : Node
  deriving Repr, BEq, Inhabited, Hashable


inductive AssumptionKind
  | eq (name : String) (value : Node)
  deriving Repr, BEq, Inhabited
