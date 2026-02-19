import Latex2Lean.CategorizedFormula
import Latex2Lean.Analysis
import Latex2Lean.LeanCmd

import Mathlib.Data.Finset.Basic
import Mathlib.Data.Finset.Card
import Mathlib.Data.Set.Basic

import Lean

/-!
About translating the formulas into lean commands, to insert into the user's
code.
-/


namespace Latex2Lean

open Lean
open Lean.Meta
open Lean.Elab.Term

private abbrev CF := CategorizedFormula
private abbrev F := Formula
private abbrev Name := Array Char
private abbrev M := ReaderT Analysis TermElabM


instance : MonadLift CoreM M where monadLift := fun x _ => x
instance : MonadLift MetaM M where monadLift := fun x _ => x


-- Helpers


private def isFiniteSet (name : Name) : M Bool := do
  let isFiniteSet := (← read).isFiniteSet
  return isFiniteSet.contains ⟨name, []⟩

private def mustBeFiniteSet (name : Name) : M Bool := do
  let mustBeFiniteSet := (← read).mustBeFiniteSet
  return mustBeFiniteSet.contains ⟨name, []⟩


private def varToIdent (name : Name) : Ident := mkIdent (.mkSimple name)

private def varToTerm (name : Name) : M Term := ``($(varToIdent name))

private def varToExpr (name : Name) : M Expr := do
  elabTermEnsuringType (← varToTerm name) none


private def empty (t : Option Expr) : M Expr := do
  elabTermEnsuringType (← ``(∅)) t


mutual


private partial def asNumber : F → M Expr
  | .var name .. => varToExpr name
  | .number n .. => return mkNatLit n
  | .abs inner .. => do
    -- TODO: What if inner is actually a number?
    let innerExpr ← asFinset inner
    mkAppM ``Finset.card #[innerExpr]
  | .binOp left op right .. => do
    let leftExpr ← asNumber left
    let rightExpr ← asNumber right
    let f ← match op with
      | .plus => pure ``HAdd.hAdd
      | .star => pure ``HMul.hMul
      | _ => throwError s!"unsupported binary operator for translation to number: {repr op}"
    mkAppM f #[leftExpr, rightExpr]
  | f => throwError s!"unsupported formula for translation to number: {repr f}"


private partial def asFinset : F → M Expr
    | .emptySet .. => mkAppM ``Finset.empty #[]
    | .var name .. => varToExpr name
    | .number n .. => throwError s!"cannot translate number {n} into a finset"
    | .abs .. => throwError s!"cannot translate absolute value into a finset"
    -- | .binOp (left : Formula) (op : BinOp) (right : Formula)
    | .simpleSet elements .. => do
      let elements ← elements.mapM asWhatever
      let list ← mkListLit (←mkFreshTypeMVar) elements.toList
      check list
      mkAppM ``List.toFinset #[list]
    -- | .mapSet (lhs : Formula) (binders : Array Formula.Binder) (range : Range)
    -- | .tuple (elements : Array Formula) (range : Range)
    | f => throwError s!"unsupported formula for translation to finset: {repr f}"


private partial def asSet : F → M Expr
  | .emptySet .. => do empty $ some $ ← setType
  | .var name .. => varToExpr name
  | .number n .. => throwError s!"cannot translate number {n} into a set"
  | .abs .. => throwError s!"cannot translate absolute value into a set"
  | .binOp left op right .. => do
    let leftExpr ← asSet left
    let rightExpr ← asSet right
    let f ← match op with
      | .cap => pure ``Set.inter
      | .cup => pure ``Set.union
      | _ => throwError s!"unsupported binary operator for translation to set: {repr op}"
    mkAppM f #[leftExpr, rightExpr]
  | .simpleSet elements range => do
    if elements.isEmpty
    then asSet (.emptySet range)
    else
      let elements ← elements.mapM (asWhatever · >>= liftM ∘ exprToSyntax)
      let separated : Syntax.TSepArray `term "," := .ofElems elements
      let stx ← ``(({ $separated:term,* } : Set _))
      elabTermEnsuringType stx (some (← setType))
  | f => throwError s!"unsupported formula for translation to set: {repr f}"
where
  setType : M Expr := do
    let elementType ← mkFreshTypeMVar
    let elementStx ← exprToSyntax elementType
    elabType (← ``(Set $elementStx))


private partial def asTuple (f : F) : M Expr :=
  panic! "asTuple"


private partial def asWhatever (f : F) : M Expr :=
  match f with
  | .emptySet .. => asSet f
  | .var name .. => varToExpr name
  | .number .. => asNumber f
  | .abs .. => asNumber f
  | .binOp left op right => do
    let left ← asWhatever left
    let left ← exprToSyntax left
    let right ← asWhatever right
    let right ← exprToSyntax right
    let stx ← match op with
      | .plus => ``($left + $right)
      | .minus => ``($left - $right)
      | .star => ``($left * $right)
      | .slash => ``($left / $right)
      | .cap => ``($left ∩ $right)
      | .cup => ``($left ∪ $right)
      | .eq => ``($left = $right)
      | .in_ => ``($left ∈ $right)
    elabTermEnsuringType stx none
  | .simpleSet .. => asSet f
  | .mapSet .. => asSet f
  | .tuple .. => asTuple f


end


/-- Translate a definition. Needs to decide the type to translate into. -/
private def definition (name : Name) (f : F) : M LeanCmd := do
  let leanName := Name.mkSimple name
  if ← mustBeFiniteSet name
  then
    if ← isFiniteSet name
    then return .def_ leanName (← asFinset f)
    else return panic!" tarstastarstat!!!!!!!! "
  else
    return .def_ leanName (← asWhatever f)


/-- Translate an axiom. Needs to translate into a proposition. -/
private def axiom_ (f : F) : M LeanCmd :=
  panic! "axiom_"

private def categorizedFormula : CF → M (Option LeanCmd)
  | .definition name _ e => return some (← definition name e)
  | .axiom_ f => return some (← axiom_ f)
  | .plain .. => return none


def translate (f : CF) (a : Analysis) : TermElabM (Option LeanCmd) :=
  categorizedFormula f a
