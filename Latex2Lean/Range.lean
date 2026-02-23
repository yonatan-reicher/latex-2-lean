import Latex2Lean.Pos


namespace Latex2Lean


structure Range where
  left : Pos
  right : Pos
  -- TODO: left <= right
  deriving DecidableEq, Repr, Inhabited


namespace Range


def toString : Range → String
  | .mk (Pos.mk l1 c1) (Pos.mk l2 c2) =>
    if l1 = l2
    then s!"[{l1}:{c1}-{c2}]"
    else s!"[{l1}-{l2}:{c1}-{c2}]"


instance : ToString Range where
  toString := Range.toString


def merge (r1 r2 : Range) : Range :=
  Range.mk (min r1.left r2.left) (max r1.right r2.right)

instance : Union Range where
  union := Range.merge
