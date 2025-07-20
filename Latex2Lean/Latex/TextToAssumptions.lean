import Latex2Lean.Assumption.Basic
import Latex2Lean.Assumption.ToAssumptionKind
import Latex2Lean.Latex.TextMode
import Latex2Lean.Latex.MathToNode


open Lean (TSyntax)


partial def LatexText.toAssumptions : LatexText -> Except BadLatex (List Assumption)
  | `(latexText| $atoms:latexTextAtom*) =>
    atoms.toList.flatMapM atom
  | _ => return []
where
  atom : TSyntax _ -> Except BadLatex (List Assumption)
    | `(latexTextAtom| $ $math:latexMath $) 
    | `(latexTextAtom| $$ $math:latexMath $$) => do
      let math : LatexMath := math
      let node <- math.toNode
      let assumption := Assumption.mk node
      let isValid := assumption.toAssumptionKind.isOk
      return if isValid then [assumption] else []
    | `(latexTextAtom| notMath) => return []
    | _ => return []
