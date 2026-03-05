import Latex2Lean.Util
import Latex2Lean.Formula
import Latex2Lean.CategorizedFormula


namespace Latex2Lean

def categorize : Formula → CategorizedFormula
  | .binOp (.var name nameRange) .eq right => .definition name nameRange right
  | f@(.binOp _ op _) => if op.predicative then .axiom_ f else .plain f
  | f => .plain f


#guard
  categorize (.binOp (.var "X" default) .subseteq (.var "Y" default))
  |> fun | .axiom_ _ => true | _ => false
