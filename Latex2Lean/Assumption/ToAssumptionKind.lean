import Latex2Lean.Assumption.Basic
import Latex2Lean.Node.Asserts
import Latex2Lean.Node.Basic


def Assumption.toAssumptionKind (a : Assumption) : Except String AssumptionKind
:= do
  match a.expr.name with
  | "=" =>
    let (name, value) <- a.expr.assert2Children
    name.assert0Children
    return .eq name.name value
  | _ => throw "Not implemented"
