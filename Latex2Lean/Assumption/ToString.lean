import Latex2Lean.Assumption.Basic
import Latex2Lean.Node.ToString


def Assumption.toString (a: Assumption) : String :=
  s!"[ {a.expr.toString} ]"


instance : ToString Assumption where
  toString := Assumption.toString
