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
    let mustBeFiniteSet <- ExceptT.lift $ mustBeFiniteSet term
    if mustBeFiniteSet then ``( ($inner : Finset _) )
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
    let (lhs, rhs) <- assert2Children term
    let lhs <- lhs.toTerm
    let (v, s) <- assert2Children rhs
    assert0Children v
    let v := Lean.mkIdent (.mkSimple v.name)
    let s <- s.toTerm
    ``( { $lhs:term | $v:ident ∈ $s:term } )
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
