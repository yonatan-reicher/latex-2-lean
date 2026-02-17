import Latex2Lean.CategorizedFormula
import Latex2Lean.Analysis
import Latex2Lean.LeanCmd

import Mathlib.Data.Finset.Basic
import Mathlib.Data.Set.Basic

import Lean

/-!
About translating the formulas into lean commands, to insert into the user's
code.
-/


namespace Latex2Lean


private abbrev CF := CategorizedFormula
private abbrev F := Formula
private abbrev Name := Array Char
private abbrev M := ReaderT Analysis Lean.Elab.TermElabM


private def isFiniteSet (name : Name) : M Bool := do
  let isFiniteSet := (← read).isFiniteSet
  return isFiniteSet.contains ⟨name, []⟩

private def mustBeFiniteSet (name : Name) : M Bool := do
  let mustBeFiniteSet := (← read).mustBeFiniteSet
  return mustBeFiniteSet.contains ⟨name, []⟩


private def asFinset (f : F) : M Lean.Expr := do
  let elementMVar ← Lean.Meta.mkFreshTypeMVar
  let elementTerm ← Lean.Elab.Term.exprToSyntax elementMVar
  let finset ← Lean.Elab.Term.elabType (← ``(Finset $elementTerm))
  let stx <- match f with
    | .emptySet .. => ``(Finset.empty)
    | .var name .. => pure <| Lean.mkIdent (.mkSimple name)
    | _ => panic "a"
  Lean.Elab.Term.elabTermEnsuringType stx finset


private def asSet (f : F) : M Lean.Expr :=
  panic! "asSet"


/-- Translate a definition. Needs to decide the type to translate into. -/
private def definition (name : Name) (f : F) : M LeanCmd := do
  let leanName := Lean.Name.mkSimple name
  if ← mustBeFiniteSet name
  then
    if ← isFiniteSet name
    then return .def_ leanName (← asFinset f)
    else return panic!" tarstastarstat!!!!!!!! "
  else
    return .def_ leanName (← asSet f)


/-- Translate an axiom. Needs to translate into a proposition. -/
private def axiom_ (f : F) : M LeanCmd :=
  panic! "axiom_"

private def categorizedFormula : CF → M (Option LeanCmd)
  | .definition name _ e => return some (← definition name e)
  | .axiom_ f => return some (← axiom_ f)
  | .plain .. => return none


def translate (f : CF) (a : Analysis) : Lean.Elab.TermElabM (Option LeanCmd) :=
  categorizedFormula f a
