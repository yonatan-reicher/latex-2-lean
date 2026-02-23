import Latex2Lean.CategorizedFormula
import Latex2Lean.Analysis
import Latex2Lean.LeanCmd

import Mathlib.Data.Finset.Basic
import Mathlib.Data.Finset.Card
import Mathlib.Data.Set.Basic

import Lean
import Batteries.Util.ExtendedBinder

/-!
About translating the formulas into lean commands, to insert into the user's
code.
-/


namespace Latex2Lean

open Batteries.ExtendedBinder
open Lean
open Lean.Elab.Term
open Lean.Meta

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

private def varToTerm (name : Name) : TermElabM Term := ``($(varToIdent name))

private def varToExpr (name : Name) : TermElabM Expr := do
  elabTermEnsuringType (← varToTerm name) none


private def empty (t : Option Expr) : M Expr := do
  elabTermEnsuringType (← ``(∅)) t


/-- Taken from:
https://github.com/leanprover/lean4/blob/985f350dcd18fc7814dfa677cac09933f44f3215/src/Lean/Meta/ProdN.lean#L42
-/
private def mkProdMkN (es : Array Expr) : MetaM (Expr × Expr) := do
  if h : es.size > 0 then
    let mut tuple := es.back
    let mut tupleTy ← inferType tuple
    let mut u ← getDecLevel tupleTy
    let mut es := es.pop
    for i in 0...es.size do
      let e := es.back!
      let ty ← inferType e
      let u' ← getDecLevel ty
      tuple := mkApp4 (mkConst ``Prod.mk [u', u]) ty tupleTy e tuple
      tupleTy := mkApp2 (mkConst ``Prod [u', u]) ty tupleTy
      u := (mkLevelMax u u').normalize
      es := es.pop
    return (tuple, tupleTy)
  else
    let u ← mkFreshLevelMVar
    return (mkConst ``PUnit.unit [u], mkConst ``PUnit [u])


private def mkExistsFVars (names : Array Expr) (body : Expr) : M Expr := do
  let mut result := body
  for name in names.reverse do
    result ← mkAppM ``Exists $ Array.singleton $ ← mkLambdaFVars #[name] result
  return result


/-- Get some type that has a single parameter. Returns both the type and the
  meta variable of the argument. -/
private def getAppliedType' (name : Lean.Name) : MetaM (Expr × Expr) := do
  let arg ← mkFreshTypeMVar
  let u ← mkFreshLevelMVar
  return (mkApp (mkConst name [u]) arg, arg)

private def setType' : MetaM (Expr × Expr) := getAppliedType' ``Set
private def setType : MetaM Expr := Prod.fst <$> setType'
private def finsetType' : MetaM (Expr × Expr) := getAppliedType' ``Finset
private def finsetType : MetaM Expr := Prod.fst <$> finsetType'


private def getSetOrFinsetElement (e : Expr) : M (Option Expr) :=
  withNewMCtxDepth do
    let (setType, setElementType) ← setType'
    let (finsetType, finsetElementType) ← finsetType'
    let outMVar ← do
      if ← isDefEq e setType then pure setElementType
      else if ← isDefEq e finsetType then pure finsetElementType
      else failure
    return ← instantiateMVars outMVar


mutual


/-- Translate a binder to an exists expression. -/
@[inline]
private partial def binderToExists : Formula.Binder → (rhs : M Expr) → M Expr
  | .in_ name set, rhs => do
    -- First translate the set, and extract the element type.
    let set ← asWhatever set
    check set -- Must call this before the next action!
    let type ← inferType set
    let some elementType ← getSetOrFinsetElement type
      | throwError m!"{set} must be a set, but had type {type}."
    -- Declare the variable!
    withLocalDeclD (.mkSimple name) elementType fun fvar => do
      -- Now make some syntax.
      mkAppM ``Exists $ Array.singleton $ ← mkLambdaFVars #[fvar] $
      mkAnd (← mkAppM ``Membership.mem #[set, fvar]) (← rhs)


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
  | f => throwError s!"unsupported formula for translation to number: {f}"


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
  | .mapSet _ #[] .. => throwError s!"mapSet with no binders" -- TODO
  | .mapSet lhs binders .. => do
    -- We need to generate calls to finset operations and assume that the things
    -- given can be translated to finsets. For `{ x + 1 | x \in A }`, we want to
    -- use `Finset.image`, like this `A.image fun x => x + 1`. For multiple
    -- bindings, we want to use `Finset.product` to make tuples first, then you
    -- get something like `(A.product B).image fun (x, y) => x + y`.
    let b ← match binders with
      | #[b] => pure b
      | _ => throwError m!"not supported yet"
    let .in_ name set := b
    -- Get the element type
    let set ← asFinset set
    check set
    let t ← inferType set
    let some elementType ← getSetOrFinsetElement t
      | throwError m!"{set} must be a finset, but had type {t}."
    -- Declare a local
    withLocalDeclD (.mkSimple name) elementType fun fvar => do
      -- Return final expression
      mkAppM ``Finset.image $ (#[·, set]) $
        ← mkLambdaFVars #[fvar] $ ← asWhatever lhs
  | f => throwError s!"unsupported formula for translation to finset: {f}"


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
  | .mapSet _ #[] .. => throwError s!"mapSet with no binders" -- TODO
  | .mapSet lhs binders .. => do
    -- We want to generate `{ lhs | (x ∈ A) (y ∈ B) }`. This is actually pretty
    -- hard to generate this as syntax, because of how free variables interact
    -- with syntax and the expressions. So instead we generate
    -- `setOf fun a => Exists fun x => x ∈ A ∧ Exists fun y => y ∈ B ∧ a = lhs`.
    let aName := `a
    let aType ← mkFreshTypeMVar
    mkAppM ``setOf $ Array.singleton $
      ← withLocalDeclD aName aType fun aFVar => do
        mkLambdaFVars #[aFVar] $ ← do
          let pred ← binders.foldl
            (β := M Expr)
            (init := do mkEq aFVar $ ← asWhatever lhs)
            fun acc b => binderToExists b acc
          check pred
          return pred
  | f => throwError s!"unsupported formula for translation to set: {f}"
where


private partial def asTuple : F → M Expr
  | .var name .. => varToExpr name
  | .tuple elements .. => do
    let elements ← elements.mapM asWhatever
    Prod.fst <$> mkProdMkN elements
  | f => throwError s!"unsupported formula for translation to tuple: {f}"


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
    else throwError s!"'{Name.mkSimple name}' must be a Finset but could not be inferred as finite"
  else
    return .def_ leanName (← asWhatever f)


/-- Translate an axiom. Needs to translate into a proposition. -/
private def axiom_ (f : F) : M LeanCmd := do
  -- TODO: Turns out that `getUnusedName` only returns a name not used in the
  -- local context, so we can still get name clashes (because our names get
  -- added to the global scope). Fix this!
  let name := (← getLCtx).getUnusedName (.mkSimple "h")
  return .axiom_ name $ ← asWhatever f

private def categorizedFormula : CF → M (Option LeanCmd)
  | .definition name _ e => return some (← definition name e)
  | .axiom_ f => return some (← axiom_ f)
  | .plain .. => return none


def translate (f : CF) (a : Analysis) : TermElabM (Option LeanCmd) :=
  categorizedFormula f a
