/-
This file contains not only the `Node` type, but all types related to
understanding nodes and other things from the souffle code.
-/

/--
A node like [ +, [ [ 1, nil ], [ [ 2, nil ], nil ] ] ].
This is represented as ⟨"+", [⟨"1", []⟩, ⟨"2", []⟩]⟩.
-/
structure Node where
  name : String
  children : List Node
  deriving Repr, BEq, Inhabited, Hashable

structure Assumption where
  expr : Node
  deriving Repr, BEq, Inhabited, Hashable


inductive AssumptionKind
  | eq (name : String) (value : Node)
  deriving Repr, BEq, Inhabited
