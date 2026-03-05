import Latex2Lean.Util
import Latex2Lean.LeanCmd

import Lean


namespace Latex2Lean

open Lean (addDecl instantiateMVars)
open Lean.Meta (inferType check isProp)
open Lean.Elab (TermElabM)

def emit : LeanCmd → TermElabM Unit
  | .def_ name e => do
    let e ← instantiateMVars e
    check e
    let type <- inferType e
    addDecl $ .defnDecl {
      name := name
      type := type
      value := e
      levelParams := []
      hints := default -- TODO
      safety := .safe
    }
  | .axiom_ name e => do
    check e
    if not <| ← isProp e then
      let t ← inferType e
      throwError m!"axioms must define propositions, but this one defines a '{t}'"
    Lean.Elab.Term.synthesizeSyntheticMVarsNoPostponing
    addDecl $ .axiomDecl {
      name := name
      type := ← instantiateMVars e
      levelParams := []
      isUnsafe := false
    }
