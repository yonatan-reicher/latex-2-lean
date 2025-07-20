import Latex2Lean.Assumption.Basic
import Latex2Lean.Assumption.ToAssumptionKind
import Latex2Lean.Node.ToTerm
import Lean


open Lean (Command CoreM Name Syntax TSyntax)


/-!
This file is about taking assumptions and making the _happen_.
-/
def Assumption.toCommand (a : Assumption) : AnalysisReaderT CoreM Command := do
  match a.toAssumptionKind with
  | .error m => throwError m
  | .ok (.eq name value) =>
    let name <- `(ident|$(Name.mkSimple name))
    let expr <- value.toTerm
    let expr <- Lean.ofExcept expr
    `(def $name := $expr)
