namespace Latex2Lean


structure Pos where
  line : Nat
  col : Nat
  deriving DecidableEq, Repr


def Pos.initial : Pos where
  line := 1
  col := 1


instance : Inhabited Pos where
  default := Pos.initial


def Pos.toString : Pos → String
  | { line, col } => s!"[{line}:{col}]"
instance : ToString Pos where
  toString := Pos.toString


structure Range where
  left : Pos
  right : Pos
  deriving DecidableEq, Repr


def Range.toString : Range → String
  | .mk (.mk l1 c1) (.mk l2 c2) =>
    if l1 = l2
    then s!"[{l1}:{c1}-{c2}]"
    else s!"[{l1}-{l2}:{c1}-{c2}]"


instance : ToString Range where
  toString := Range.toString
