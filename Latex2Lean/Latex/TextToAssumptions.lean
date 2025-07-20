import Latex2Lean.Assumption.Basic
import Latex2Lean.Assumption.ToAssumptionKind
import Latex2Lean.Latex.TextMode
import Latex2Lean.Latex.MathToNode


def LatexText.toAssumptions : LatexText -> Except BadLatex (List Assumption)
  | `(latexText| $ $math:latexMath $) 
  | `(latexText| $$ $math:latexMath $$) => do
    let math : LatexMath := math
    let node <- math.toNode
    let assumption := Assumption.mk node
    let isValid := assumption.toAssumptionKind.isOk
    if isValid then return [assumption]
    else return []
  | `(latexText| notMath)
  | _ => return []
