import Latex2Lean.Assumption.Basic
import Latex2Lean.Assumption.ToAssumptionKind
import Latex2Lean.Latex.TextMode
import Latex2Lean.Latex.MathMode


open Lean (TSyntax)


namespace Latex


partial def textToNodes (text : Substring)
: Except Error (Array Node) := do
  let inners := getMathModeText text
  inners.mapM (m := Except Error) fun s => do
    match mathModeForSure.parse (.ofString s.toString) with
    | .ok x _ => return x
    | .fail f _ => nomatch f
    | .error e _ => throw e
