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


end Pos
