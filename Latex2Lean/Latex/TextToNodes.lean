import Latex2Lean.Assumption.Basic
import Latex2Lean.Assumption.ToAssumptionKind
import Latex2Lean.Latex.TextMode
import Latex2Lean.Latex.MathToNode


open Lean (TSyntax)


partial def LatexText.toNodes : LatexText -> Except BadLatex (List Node)
  | `(latexText| $atoms:latexTextAtom*) =>
    atoms.toList.flatMapM atom
  | _ => return []
where
  atom : TSyntax _ -> Except BadLatex (List Node)
    | `(latexTextAtom| $ $math:latexMath $) 
    | `(latexTextAtom| $$ $math:latexMath $$) => do
      let math : LatexMath := math
      let node <- math.toNode
      return [node]
    | `(latexTextAtom| notMath) => return []
    | _ => return []
