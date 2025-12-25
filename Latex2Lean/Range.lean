import Latex2Lean.Pos


namespace Latex2Lean


structure Range where
  left : Pos
  right : Pos
  -- TODO: left <= right
  deriving DecidableEq, Repr


namespace Range


def toString : Range → String
  | .mk (Pos.mk l1 c1) (Pos.mk l2 c2) =>
    if l1 = l2
    then s!"[{l1}:{c1}-{c2}]"
    else s!"[{l1}-{l2}:{c1}-{c2}]"


instance : ToString Range where
  toString := Range.toString
