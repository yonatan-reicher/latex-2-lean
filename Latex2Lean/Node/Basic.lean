namespace Latex2Lean


/--
A node like [ +, [ [ 1, nil ], [ [ 2, nil ], nil ] ] ].
This is represented as ⟨"+", [⟨"1", []⟩, ⟨"2", []⟩]⟩.
-/
structure Node where
  name : String
  children : List Node := []
  deriving Repr, BEq, Inhabited, Hashable
