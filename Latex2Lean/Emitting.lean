import Latex2Lean.Util
import Latex2Lean.LeanCmd

import Lean


namespace Latex2Lean

open Lean (
  mkIdent
)
open Lean.Elab.Term (
  exprToSyntax
)
open Lean.Elab.Command (
  CommandElabM
  liftTermElabM
  liftCoreM
  elabCommand
)


def emit : LeanCmd → TermElabM Unit
  | .def_ name e => do
    let e : Lean.Term ← liftTermElabM <| exprToSyntax e
    let c : Lean.Command ← `(def $(mkIdent name) := $e)
    elabCommand c
    -- let decl := Lean.Declaration.defnDecl {
    --   name := name,
    --   levelParams := [],
    --   type := ← liftTermElabM (Lean.Meta.inferType e),
    --   value := e,
    --   hints := default,
    --   safety := .safe
    -- }
    -- liftCoreM (Lean.addDecl decl)
  | .axiom_ e => panic! "a"
