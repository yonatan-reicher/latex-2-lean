import Latex2Lean.Assumption.Basic
import Latex2Lean.Assumption.ToAssumptionKind
import Latex2Lean.Latex.TextMode
import Latex2Lean.Latex.MathToNode


partial def LatexText.toAssumptions : LatexText -> Except BadLatex (List Assumption)
  | `(latexText| $ $math:latexMath $ $rest) 
  | `(latexText| $$ $math:latexMath $$ $rest) => do
    let math : LatexMath := math
    let rest : LatexText := rest
    let node <- math.toNode
    let assumption := Assumption.mk node
    let isValid := assumption.toAssumptionKind.isOk
    let toAdd := if isValid then [assumption] else []
    return toAdd ++ (<-rest.toAssumptions)
  | `(latexText| notMath $rest) => LatexText.toAssumptions rest
  | _ => return []
