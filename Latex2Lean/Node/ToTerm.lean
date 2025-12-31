import Latex2Lean.Analysis.Basic
import Latex2Lean.Analysis.Monad
import Latex2Lean.Node.Asserts
import Latex2Lean.Node.Basic
import Latex2Lean.Node.ToString
import Latex2Lean.Except
import Lean
import Mathlib.Data.Finset.Defs
import Mathlib.Data.Finset.Card
import Mathlib.Data.Set.Defs
import Std


open Lean (
  CoreM
  Syntax
  TSyntax
  Term
)

open Batteries.ExtendedBinder (
  extBinder
  extBinderCollection
  extBinderParenthesized
  extBinders
)


abbrev M := ExceptT String (AnalysisReaderT CoreM)


private def mapToSetTerm
  (func : Term) (mappings : Array (Lean.Ident × Lean.Term)) : M Term := do
  -- Translate to syntax
  let binderList <-
    mappings.mapM fun
      | (v, s) => `(extBinderParenthesized| ($v:ident ∈ $s:term) )
  -- Collect together
  let binderCollection : TSyntax ``extBinderCollection <-
    `(extBinderCollection| $binderList* )
  let binders <- `(extBinders| $binderCollection:extBinderCollection )
  ``( { $func:term | $binders:extBinders } )

private def mapToFinsetTerm
  (func : Term) (mappings : Array (Lean.Ident × Term)) : M Term := do
  let sets := mappings.map fun (_, s) => s
  let vars := mappings.map fun (v, _) => v
  let setsHead := sets[0]!
  let setsTail := sets[1:]
  let sets ← setsTail.foldlM (init := setsHead) fun acc s => ``($acc × $s)
  let vars ← vars.foldlM (init := vars[0]!) fun acc v => ``( ($acc, $v) )
  ``( Finset.image (fun y => $func:term) $sets:term )


partial def Node.toTerm (term : Node)
: AnalysisReaderT CoreM $ Except String Term := ExceptT.run do
  -- First check term's name for a number
  if term.name.isNat then
    term.assert0Children
    return Syntax.mkNumLit term.name
  -- Otherwise, it should be some operation or a variable
  match term.name with
  | "new-set" =>
    let children <- term.children.mapM toTerm
    let children : Syntax.TSepArray `term "," := .ofElems children.toArray
    let inner <- if children.elemsAndSeps.isEmpty then ``({}) else ``({ $children:term,* })
    -- let mustBeFiniteSet <- ExceptT.lift $ mustBeFiniteSet term
    -- if mustBeFiniteSet then ``( ($inner : Finset _) )
    let isFiniteSet <- ExceptT.lift $ isFiniteSet term
    if isFiniteSet then ``( ($inner : Finset _) )
    else ``( ($inner : Set _) )
  | "tuple" =>
    let children <- term.children.mapM toTerm
    match children with
    | [] => ``( () )
    | [_] => panic! "unsupported"
    | h :: t =>
      let t : Syntax.TSepArray `term "," := .ofElems t.toArray
      ``( ($h:term, $t:term,*) )
  | "union" => binOp term (fun l r => ``($l ∪ $r))
  | "intersect" => binOp term (fun l r => ``($l ∩ $r))
  | "+" => binOp term fun l r => ``($l + $r)
  | "-" => binOp term fun l r => ``($l - $r)
  | "*" => binOp term fun l r => ``($l * $r)
  | "/" => binOp term fun l r => ``($l / $r)
  | "in" => binOp term fun l r => ``($l ∈ $r)
  | "notin" => binOp term fun l r => ``(¬ $l ∈ $r)
  | "cross" => binOp term fun l r => ``($l × $r)
  | "subseteq" => binOp term fun l r => ``($l ⊆ $r)
  | "subset" => binOp term fun l r => ``($l ⊂ $r)
  | "supseteq" => binOp term fun l r => ``($l ⊇ $r)
  | "supset" => binOp term fun l r => ``($l ⊃ $r)
  | "abs" => unaOp term (fun a => `(Finset.card $a))
  | "map" =>
    match term.children with
    | []  | [_] => panic! "bad"
    | lhs :: rhs =>
      -- The left-hand side is what is being returned for each element
      let lhs <- lhs.toTerm
      -- The right-had side is the "mappings" - a collection of things of the
      -- form "x ∈ A".
      let mappings : Array (Lean.Ident × Lean.Term) <-
        rhs.toArray.mapM fun mapping => do
          let (v, s) <- assert2Children mapping
          assert0Children v
          let v := Lean.mkIdent (.mkSimple v.name)
          let s <- s.toTerm
          return (v, s)
      let mustBeFiniteSet <- ExceptT.lift $ mustBeFiniteSet term
      if mustBeFiniteSet
      then mapToFinsetTerm lhs mappings
      else mapToSetTerm lhs mappings
  | var =>
    match term.children with
    | [] =>
      -- This must be the name of some variable
      return Lean.mkIdent (.mkSimple var) 
    | _ =>
      -- This is must have been an operation, which we haven't implemented a
      -- translation to a term for.
      let argsString := term.children.map (s!"\n* {·}") |> "".intercalate
      panic! s!"Unimplemented operation '{term.name}', given arguments {argsString}"
where
  binOp
  (term : Node)
  (op : Term -> Term -> CoreM Term)
  : AnalysisReaderT CoreM $ Except String Term := ExceptT.run do
    let (lhs, rhs) <- term.assert2Children
    let lhs <- lhs.toTerm
    let rhs <- rhs.toTerm
    op lhs rhs
  unaOp
  (term : Node)
  (op : Term -> CoreM Term)
  : AnalysisReaderT CoreM $ Except String Term := ExceptT.run do
    let x <- term.assert1Children
    let x <- x.toTerm
    op x


section

    
open Lean Elab Term Command Meta in
elab "test " node:term " eq " stx:term : command => do
  let node <- liftTermElabM $ unsafe evalTerm Node (.const ``Node []) node
  let term? <- liftCoreM $ node.toTerm.run default
  let term <- Lean.ofExcept term?
  liftTermElabM $ do
    if not $ <- isDefEq (<- elabTerm term none) (<- elabTerm stx none) then
      throwError
        m!"Node '{node.toString}' should translate to '{stx}', but got '{term}' instead."
  /-
`(#eval return (<- ($node : Node).toTerm.run default) == Except.ok (<- $stx))
-/

test ⟨"2", []⟩ eq 2
/-- info: Except.error "Wrong number of arguments to '+' - expected 2, got 0" -/
#guard_msgs in
#eval (Node.mk "+" []).toTerm.run default
test ⟨"+", [⟨"1", []⟩, ⟨"2", []⟩]⟩ eq 1 + 2
test ⟨"new-set", [⟨"1", []⟩, ⟨"2", []⟩]⟩ eq ({1, 2} : Set _)
-- test ⟨"union", [⟨"new-set", [⟨"1", []⟩, ⟨"2", []⟩]⟩, ⟨"1", []⟩]⟩
--   eq (({1, 2} : Set _) ∪ 1)


end
