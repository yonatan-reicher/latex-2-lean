import Latex2Lean.Util
import Latex2Lean.Formula
import Latex2Lean.CategorizedFormula


namespace Latex2Lean

def categorize : Formula → CategorizedFormula
  | .binOp (.var name nameRange) .eq right => .definition name nameRange right
  | f@(.binOp _ .in_ _)
  | f@(.binOp _ .eq _) => .axiom_ f
  | f => .plain f
