import Latex2Lean.Analysis.Basic
import Latex2Lean.Analysis.Monad
import Latex2Lean.Node.Asserts
import Latex2Lean.Node.Basic
import Latex2Lean.Except
import Lean
import Mathlib
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
  -- Otherwise, it should be some operation
  match term.name with
  | "+" =>
    let (lhs, rhs) <- term.assert2Children
    let lhs : Term <- lhs.toTerm
    let rhs : Term <- rhs.toTerm
    ``($lhs + $rhs)
  | "new-set" =>
    let children <- term.children.mapM toTerm
    let children : Syntax.TSepArray `term "," := .ofElems children.toArray
    let inner <- ``({ $children:term,* })
    let mustBeFiniteSet <- ExceptT.lift $ mustBeFiniteSet term
    if mustBeFiniteSet then ``( (($inner) : Finset _) )
    else ``( (($inner) : Set _) )
  | "union" => binOp term (fun l r => ``($l ∪ $r))
  | "intersect" => binOp term (fun l r => ``($l ∩ $r))
  | var =>
    -- This must be the name of some variable
    return Syntax.mkNameLit var
where
  binOp
  (term : Node)
  (op : Term -> Term -> CoreM Term)
  : AnalysisReaderT CoreM $ Except String Term := ExceptT.run do
    let (lhs, rhs) <- term.assert2Children
    let lhs <- lhs.toTerm
    let rhs <- rhs.toTerm
    op lhs rhs


section

    
macro "test " node:term " eq " stx:term : command =>
  `(#eval return (<- ($node : Node).toTerm.run default) == Except.ok (<- $stx))

test ⟨"2", []⟩ eq `(2)
#eval (Node.mk "+" []).toTerm.run default
#eval (Node.mk "+" [Node.mk "1" [], Node.mk "2" []]).toTerm.run default
#eval (Node.mk "new-set" [Node.mk "1" [], Node.mk "2" []]).toTerm default
#eval (Node.mk "new-set" [Node.mk "1" [], Node.mk "2" []]).toTerm default
#eval (Node.mk "union" [Node.mk "1" [], Node.mk "2" []]).toTerm default


end
