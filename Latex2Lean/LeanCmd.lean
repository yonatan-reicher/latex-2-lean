import Lean.Expr
import Lean.PrettyPrinter


namespace Latex2Lean

open Lean (instantiateMVars)
open Lean.Meta (inferType ppExpr)


inductive LeanCmd where
  | def_ (name : Lean.Name) (e : Lean.Expr)
  | axiom_ (e : Lean.Expr)
  deriving Inhabited, Repr, BEq


def LeanCmd.prettyPrint : LeanCmd → Lean.MetaM String
  | .def_ name e => do
    let e ← instantiateMVars e
    let typ ← inferType e
    return s!"def {name} : {← ppExpr typ} := {← ppExpr e}"
  | .axiom_ e => return s!"axiom {← ppExpr $ ← instantiateMVars e}"
