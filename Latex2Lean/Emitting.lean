import Latex2Lean.Util
import Latex2Lean.LeanCmd

import Lean


namespace Latex2Lean

open Lean (addDecl instantiateMVars)
open Lean.Meta (inferType check)
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
  | .axiom_ e => do
    let name ← Lean.Core.mkFreshUserName (.mkSimple "h")
    check e
    let type ← inferType e
    -- synthesizeSyntheticMVars
    addDecl $ .axiomDecl {
      name := name
      type := type
      levelParams := []
      isUnsafe := false
    }
