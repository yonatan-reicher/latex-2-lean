module

public import Latex2Lean.LeanUtil
public import Latex2Lean.CategorizedFormula
public import Latex2Lean.Analysis
public import Latex2Lean.LeanCmd

-- Finset
public import Mathlib.Data.Finset.Basic
public import Mathlib.Data.Finset.Card
-- Set
public import Mathlib.Data.Set.Basic
-- Multiset
public import Mathlib.Algebra.BigOperators.Group.Multiset.Defs
-- Nat
public import Mathlib.Algebra.Group.Nat.Defs

public import Lean
public import Batteries.Util.ExtendedBinder

public section


/-!
About translating the formulas into lean commands, to insert into the user's
code.
-/


namespace Latex2Lean

open Batteries.ExtendedBinder
open Lean hiding Name
open Lean.Elab.Term
open Lean.Meta

namespace Aliases
public abbrev CF := CategorizedFormula
public abbrev F := Formula
public abbrev Name := Array Char
public abbrev M := AnalysisReaderT TermElabM
public abbrev FId := Formula.Id
end Aliases
open Aliases


instance : MonadLift CoreM M where monadLift := fun x _ => x
instance : MonadLift MetaM M where monadLift := fun x _ => x


-- Helpers

private def varToIdent (name : Name) : Ident := mkIdent (.mkSimple name)

private def varToTerm (name : Name) : TermElabM Term := ``($(varToIdent name))

private def varToExpr (name : Name) (type : Option Expr) : TermElabM Expr := do
  elabTermEnsuringType (← varToTerm name) type


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
private def multisetType' : MetaM (Expr × Expr) := getAppliedType' ``Multiset
private def multisetType : MetaM Expr := Prod.fst <$> multisetType'


/-- Get the element type of a set or a finset -/
private def getSetElement (e : Expr) : M (Option Expr) :=
  withNewMCtxDepth <| show OptionT M Expr from do
    let (setType, setElementType) ← setType'
    let (finsetType, finsetElementType) ← finsetType'
    let (multisetType, multisetElementType) ← multisetType'
    let outMVar ← do
      if ← isDefEq e setType then pure setElementType
      else if ← isDefEq e finsetType then pure finsetElementType
      else if ← isDefEq e multisetType then pure multisetElementType
      else failure
    return ← instantiateMVars outMVar


/-- Make an expression for a binary operator. Uses Syntax elaboration. Because
  of that, make sure the arguments have the correct types and that the output is
  in a context that could infer it's type -/
def binOp : BinOp → (expectedType? : Option Lean.Expr) → Expr → Expr → M Expr
  | op, t, a, b => do
    let a ← exprToSyntax a
    let b ← exprToSyntax b
    let stx ← toStx op a b
    elabTerm stx t
where toStx : BinOp → Term → Term → M Term
  | .plus, a, b => ``($a + $b)
  | .minus, a, b => ``($a - $b)
  | .star, a, b => ``($a * $b)
  | .slash, a, b => ``($a / $b)
  | .cap, a, b => ``($a ∩ $b)
  | .cup, a, b => ``($a ∪ $b)
  | .eq, a, b => ``($a = $b)
  | .in_, a, b => ``($a ∈ $b)
  | .supset, a, b => ``($a ⊃ $b)
  | .supseteq, a, b => ``($a ⊇ $b)
  | .subset, a, b => ``($a ⊂ $b)
  | .subseteq, a, b => ``($a ⊆ $b)
  | .times, a, b => ``($a × $b)
  | .land, a, b => ``($a ∧ $b)
  | .lor, a, b => ``($a ∨ $b)
  | .lt, a, b => ``($a < $b)
  | .le, a, b => ``($a ≤ $b)
  | .gt, a, b => ``($a > $b)
  | .ge, a, b => ``($a ≥ $b)


def Formula.asBinder (f : Formula) : Option Formula.Binder :=
  match f.kind with
  | .binOp (.mk varId <| .var varName varRange) .in_ rhs =>
    some <| .in_ varId f.id varName varRange rhs
  | _ => none


mutual


/-- Translate a binder to an exists expression. -/
@[inline]
private partial def binderToExists : Formula.Binder → (rhs : M Expr) → M Expr
  | .in_ (name:=name) (set:=set) .., rhs => do
    -- First translate the set, and extract the element type.
    let set ← asWhatever set
    check set -- Must call this before the next action!
    let type ← inferType set
    let some elementType ← getSetElement type
      | throwError m!"'{set}' must be a set, but had type '{type}'."
    -- Declare the variable!
    withLocalDeclD (.mkSimple name) elementType fun fvar => do
      -- Now make some syntax.
      mkAppM ``Exists $ Array.singleton $ ← mkLambdaFVars #[fvar] $
      mkAnd (← mkAppM ``Membership.mem #[set, fvar]) (← rhs)


/-- Translate a binder to an exists expression. -/
@[inline]
private partial def binderToForall : Formula.Binder → (rhs : M Expr) → M Expr
  | .in_ (name:=name) (set:=set) .., rhs => do
    -- First translate the set, and extract the element type.
    let set ← asWhatever set
    check set -- Must call this before the next action!
    let type ← inferType set
    let some elementType ← getSetElement type
      | throwError m!"'{set}' must be a set, but had type '{type}'."
    -- Declare the variable!
    withLocalDeclD (.mkSimple name) elementType fun fvar => do
      -- And declare a variable for the assumption that it is in the set.
      let inAssumption ← mkAppM ``Membership.mem #[set, fvar]
      withLocalDeclD `h inAssumption fun hFVar => do
        -- Now make some syntax.
        mkForallFVars #[fvar, hFVar] $ ← rhs


private partial def asNumber (f : F) : M Expr :=
  match f.kind with
  | .var name .. => varToExpr name none
  | .number n .. => return mkNatLit n
  | .app ⟨"\\abs", _⟩ inner => do
    let #[inner] := inner
      | throwError m!"function '\\abs' had too many arguments!"
    -- TODO: What if inner is actually a number?
    let innerExpr ← asFinset inner
    mkAppM ``Finset.card #[innerExpr]
  | .app ⟨"\\sum", _⟩ inner => do
    let #[inner] := inner
      | throwError m!"function '\\sum had too many arguments!"
    -- For now, assume the result is a multiset.
    mkAppM ``Multiset.sum #[← asMultiset inner]
  | .binOp left op right .. => do
    let leftExpr ← asNumber left
    let rightExpr ← asNumber right
    let f ← match op with
      | .plus => pure ``HAdd.hAdd
      | .star => pure ``HMul.hMul
      | _ => throwError s!"unsupported binary operator for translation to number: {repr op}"
    mkAppM f #[leftExpr, rightExpr]
  | _ => throwError s!"unsupported formula for translation to number: {f}"


private partial def asFinset (f : F) : M Expr :=
  match f.kind with
  | .emptySet .set .. => mkAppM ``Finset.empty #[]
  | .var name .. => do varToExpr name (← finsetType)
  | .number n .. => throwError s!"cannot translate number {n} into a finset"
  -- | .binOp (left : Formula) (op : BinOp) (right : Formula)
  | .simpleSet .set elements .. => do
    let elements ← elements.mapM asWhatever
    let list ← mkListLit (←mkFreshTypeMVar) elements.toList
    check list
    mkAppM ``List.toFinset #[list]
  | .set .set _ #[] .. => throwError r"'\set{ .. \mid .. }' with no binders"
  | .set .set lhs binders .. => do
    -- We need to generate calls to finset operations and assume that the things
    -- given can be translated to finsets. For `{ x + 1 | x \in A }`, we want to
    -- use `Finset.image`, like this `A.image fun x => x + 1`. For multiple
    -- bindings, we want to use `Finset.product` to make tuples first, then you
    -- get something like `(A.product B).image fun (x, y) => x + y`.
    let b ← match binders with
      | #[b] => pure b
      | _ => throwError m!"not supported yet"
    let .mk _ <| .binOp (.mk _ <| .var name _) .in_ set := b
      | panic! "unsupported"
    -- Get the element type
    let set ← asFinset set
    check set
    let t ← inferType set
    let some elementType ← getSetElement t
      | throwError m!"{set} must be a finset, but had type {t}."
    -- Declare a local
    withLocalDeclD (.mkSimple name) elementType fun fvar => do
      -- Return final expression
      mkAppM ``Finset.image $ (#[·, set]) $
        ← mkLambdaFVars #[fvar] $ ← asWhatever lhs
  | _ => throwError s!"unsupported formula for translation to finset: {f}"


private partial def asSet (f : F) : M Expr :=
  match f.kind with
  | .emptySet .set .. => do empty $ some $ ← setType
  | .var name .. => do varToExpr name (← setType)
  | .number n .. => throwError s!"cannot translate number {n} into a set"
  | .binOp left op right .. => do
    let leftExpr ← asSet left
    let rightExpr ← asSet right
    let f ← match op with
      | .cap => pure ``Set.inter
      | .cup => pure ``Set.union
      | _ => throwError s!"unsupported binary operator for translation to set: {repr op}"
    mkAppM f #[leftExpr, rightExpr]
  | .simpleSet .set elements _ => do
    let elements ← elements.mapM (asWhatever · >>= liftM ∘ exprToSyntax)
    let separated : Syntax.TSepArray `term "," := .ofElems elements
    let stx ← ``(({ $separated:term,* } : Set _))
    elabTermEnsuringType stx (some (← setType))
  | .set .set _ #[] .. => throwError r"'\set{ .. \mid .. }' with no binders"
  | .set .set lhs rhs .. => do
    let (binderAbles, nonBinders) := rhs.partition (·.asBinder.isSome)
    let binders := binderAbles.filterMap (·.asBinder)
    if binders.isEmpty then
      throwError s!"this set expression does not bind any variables"
    -- We want to generate `{ lhs | (x ∈ A) (y ∈ B) }`. This is actually pretty
    -- hard to generate this as syntax, because of how free variables interact
    -- with syntax and the expressions. So instead we generate
    -- `setOf fun a => Exists fun x => x ∈ A ∧ Exists fun y => y ∈ B ∧ a = lhs`.
    let aName := `a
    let aType ← mkFreshTypeMVar
    mkAppM ``setOf $ Array.singleton $
      ← withLocalDeclD aName aType fun aFVar => do
        mkLambdaFVars #[aFVar] $ ← do
          -- Make a predicate for the non-binders
          let pred : M Expr := do mkEq aFVar $ ← asWhatever lhs
          let pred := nonBinders
            |>.map asProp
            |>.foldr (init := pred) fun acc e => do pure $ mkAnd (← acc) (← e)
          -- Add onto it the existentials from the binders
          let pred' ← binders.foldr
            (β := M Expr)
            (init := pred)
            fun b acc => binderToExists b acc
          check pred'
          pure pred'
  | _ => throwError s!"unsupported formula for translation to set: {f}"


private partial def asMultiset (f : F) : M Expr :=
  match f.kind with
  | .emptySet .multiset .. => do empty $ some $ ← multisetType
  | .var name .. => do varToExpr name (← multisetType)
  | .binOp left op right .. => do
    let leftExpr ← asMultiset left
    let rightExpr ← asMultiset right
    let f ← match op with
      | .cap => pure ``Multiset.inter
      | .cup => pure ``Multiset.union
      | .plus => pure ``Multiset.add
      | .minus => pure ``Multiset.sub
      | _ => throwError s!"unsupported binary operator for translation to set: {repr op}"
    mkAppM f #[leftExpr, rightExpr]
  | .simpleSet .multiset elements _ => do
    -- TODO: Make this a seprate helper
    let elements ← elements.mapM (asWhatever · >>= liftM ∘ exprToSyntax)
    let separated : Syntax.TSepArray `term "," := .ofElems elements
    let stx ← ``({ $separated:term,* })
    elabTermEnsuringType stx $ some $ ← multisetType
  | .simpleSet .set .. => do
    -- You can only turn a finset into a multiset!
    let s ← asFinset f
    -- mkAppOptM ``Coe.coe #[none, ← multisetType, none, s]
    -- There is actually no coercesion instance for this conversion, so use the
    -- dumb thing.
    mkAppM ``Finset.val #[s]
  | .set .multiset _ #[] .. => throwError r"'\set{ .. \mid .. }' with no binders"
  | .set .multiset lhs binders _ => do
    -- Translate to ``Multiset.pmap, which takes 3 argumets - a mapping with a
    -- predicate, a multi-set, and a proof that the predicate holds for all the
    -- elements of the set. We don't care for the predicate, so we just give it
    -- a constant True.
    let #[
      .mk _ <| .binOp (.mk _ <| .var name _) .in_ s
    ] := binders | throwError m!"not implemented yet"
    -- This is the set
    let s ← asMultiset s
    check s
    let t ← inferType s 
    let some elementType ← getSetElement t
      | throwError m!"expected this to be a set"
    -- This is the mapping
    let f ← withLocalDeclD (.mkSimple name) elementType fun fVar => do
      -- This is the proof for the predicate on the element.
      withLocalDeclD `h (.const ``True []) fun hFVar => do
        mkLambdaFVars #[fVar, hFVar] $ ← asWhatever lhs
    -- the next variable is a proof of (∀ a ∈ s, p a) where p := fun _ => True
    let h ← withLocalDeclD `a elementType fun aFVar => do
      let mem ← mkAppM ``Membership.mem #[s, aFVar]
      withLocalDeclD `h mem fun hFVar => do
          mkLambdaFVars #[aFVar, hFVar] $ .const ``trivial []
    mkAppM ``Multiset.pmap #[f, s, h]
  | _ => throwError s!"unsupported formula for translation to multi-set: {f}"


private partial def asTuple (f : F) : M Expr :=
  match f.kind with
  | .var name .. => varToExpr name none
  | .tuple elements .. => do
    let elements ← elements.mapM asWhatever
    Prod.fst <$> mkProdMkN elements
  | _ => throwError s!"unsupported formula for translation to tuple: {f}"


private partial def asProp (f : F) : M Expr :=
  match f.kind with
  | .var name .. => varToExpr name (some <| .sort .zero)
  | .binOp .. => asWhatever f
  | .quantified q binders rhs _ => do
    -- Check
    if binders.any (·.asBinder.isNone) then
      throwError m!"cannot have non-binder on left-hand side of a '\\{q.name}'"
    -- Fold over the binders
    let quantifierBuilder := match q with
      | .forall_ => binderToForall
      | .exists_ => binderToExists
    let binders := binders.filterMap (·.asBinder)
    let f ← binders.foldr (β := M Expr) (init := asProp rhs) quantifierBuilder
    check f
    return f
  | _ => throwError s!"unsupported formula for translation to proposition: {f}"


private partial def asWhatever (f : F) : M Expr :=
  match f.kind with
  | .emptySet .set .. => asSet f
  | .emptySet .multiset .. => asMultiset f
  | .var name .. => varToExpr name none
  | .number .. => asNumber f
  | .app .. =>
    -- How could we know?? Let's try some things??
    try asNumber f
    catch e1 => try asSet f
    catch e2 => try asFinset f
    catch e3 => try asMultiset f
    catch e4 => throwError m!"Could not translate {f}.\nErrors:\n{e1}\n{e2}\n{e3}\n{e4}"
  | .binOp left op right => do
    let left ← asWhatever left
    let right ← asWhatever right
    binOp op none left right
  | .simpleSet .set .. => asSet f
  | .simpleSet .multiset .. => asMultiset f
  | .set .set .. => asSet f
  | .set .multiset .. => asMultiset f
  | .tuple .. => asTuple f
  | .forall_ .. => asProp f
  | .exists_ .. => asProp f


end


/-- Translate a definition. Needs to decide the type to translate into. -/
private def definition (id : FId) (name : Name) (f : F) : M LeanCmd := do
  let leanName := Name.mkSimple name
  if ← mustBeFiniteSet id then
    if ← isFiniteSet id
    then return .def_ leanName (← asFinset f)
    else throwError s!"'{Name.mkSimple name}' must be a Finset but could not be inferred as finite"
  else
    return .def_ leanName (← asWhatever f)


/-- Translate an axiom. Needs to translate into a proposition. -/
private def axiom_ (_id : FId) (f : F) : M LeanCmd := do
  -- TODO: Turns out that `getUnusedName` only returns a name not used in the
  -- local context, so we can still get name clashes (because our names get
  -- added to the global scope). Fix this!
  return .axiom_ none $ ← asWhatever f

private def categorizedFormula : CF → M (Option LeanCmd)
  | .definition id name _ e _varId _opId => return some (← definition id name e)
  | .axiom_ id f => return some (← axiom_ id f)
  | .plain _id .. => return none


def translate (f : CF) (a : Analysis) : TermElabM (Option LeanCmd) :=
  categorizedFormula f a
