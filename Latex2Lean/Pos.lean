namespace Latex2Lean


structure Pos where
  row : Nat
  col : Nat
  deriving DecidableEq, Repr


namespace Pos


def initial : Pos where
  row := 1
  col := 1


instance : Inhabited Pos where
  default := initial


def toString : Pos → String
  | { row, col, .. } => s!"[{row}:{col}]"
instance : ToString Pos where
  toString := toString


def advance : Char → Pos → Pos
  | '\n', { row, .. } => { row := row + 1, col := 1 }
  | _, { row, col, .. } => { row := row, col := col + 1 }


instance : LE Pos where
  le p1 p2 :=
    p1.row <= p2.row ∨ (p1.row = p2.row ∧ p1.col <= p2.col)
instance : DecidableRel (@LE.le Pos _) := by
  intros p1 p2
  simp only [(· ≤ · )]
  simp only [Nat.le_eq]
  infer_instance

instance : Min Pos where min p1 p2 := if p1 ≤ p2 then p1 else p2
instance : Max Pos where max p1 p2 := if p1 ≤ p2 then p2 else p1


#guard min (Pos.mk 1 2) (Pos.mk 2 1) = Pos.mk 1 2


end Pos
