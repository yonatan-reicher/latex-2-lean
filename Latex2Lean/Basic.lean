import Latex2Lean.AnalysisReader
import Latex2Lean.Csv
import Latex2Lean.Node
import Lean
import Mathlib
import Std


open Lean (Syntax TSyntax MetaM CoreM)
open Lean.Elab.Term (TermElabM)
open Lean.Elab.Command (CommandElabM)
open Std (HashSet)


def decidable (p : Prop) [Decidable p] : Decidable p := inferInstance


/--
Reads the analysis results in the form of Csv files as outputed by our Souffle
code.
-/
def readAnalysis (data : List Csv)
: Except String AnalysisResult := do
  let csvNames :=
    data.map (·.fileName)
    -- WHY would someone name this INTERCALATE??
    |> String.intercalate ", "
  let isFiniteSetCsvName := "isFiniteSet.csv"
  let mustBeFiniteSetCsvName := "mustBeFiniteSet.csv"

  -- Find the Csvs!
  let some isFiniteSetCsv :=
    data.find? (·.fileName == isFiniteSetCsvName)
    | throw s!"No CSV file named '{isFiniteSetCsvName}'. Found only '{csvNames}'"
  let some mustBeFiniteSetCsv :=
    data.find? (·.fileName == mustBeFiniteSetCsvName)
    | throw s!"No CSV file named '{mustBeFiniteSetCsvName}'. Found only '{csvNames}'"

  -- Matching on Decidable.isTrue let's us use these equality in tactics later
  let isTrue _ := decidable (isFiniteSetCsv.n == 1)
    | throw s!"CSV file '{isFiniteSetCsvName}' must have exactly one column, but has {isFiniteSetCsv.n} columns"
  let isTrue _ := decidable (mustBeFiniteSetCsv.n == 1)
    | throw s!"CSV file '{mustBeFiniteSetCsvName}' must have exactly one column, but has {mustBeFiniteSetCsv.n} columns"


  let isFiniteSet :=
    (<- isFiniteSetCsv.rows.mapM  fun row =>
      let nodeStr := row[0]'(by grind only)
      Node.fromString nodeStr
    )
    |> HashSet.ofArray
  let mustBeFiniteSet :=
    (<- mustBeFiniteSetCsv.rows.mapM fun row =>
      let nodeStr := row[0]'(by grind only)
      Node.fromString nodeStr
    )
    |> HashSet.ofArray

  return {
    isFiniteSet := isFiniteSet
    mustBeFiniteSet := mustBeFiniteSet
  }


partial def Node.toSyntax (term : Node)
: AnalysisReaderT CoreM $ Except String $ Lean.TSyntax `term := ExceptT.run do
  -- First check term's name for a number
  if term.name.isNat then
    term.assert0Children
    return Syntax.mkNumLit term.name
  -- Otherwise, it should be some operation
  match term.name with
  | "+" =>
    let (lhs, rhs) <- term.assert2Children
    let lhs : TSyntax `term <- lhs.toSyntax
    let rhs : TSyntax `term <- rhs.toSyntax
    ``($lhs + $rhs)
  -- | "new-set" =>
  --   let children <- term.children.mapM toSyntax
  --   let empty <- ``({})
  --   let cons xs x := ``(insert $x $xs)
  --   let inner <- children.foldlM cons empty
  --   let mustBeFiniteSet <- ExceptT.lift $ mustBeFiniteSet term
  --   if mustBeFiniteSet then ``( (($inner) : Finset _) )
  --   else ``( (($inner) : Set _) )
  | "new-set" =>
    let children <- term.children.mapM toSyntax
    let children : Lean.Syntax.TSepArray `term "," := .ofElems children.toArray
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
  (op : TSyntax `term -> TSyntax `term -> CoreM (TSyntax `term))
  : AnalysisReaderT CoreM $ Except String $ Lean.TSyntax `term := ExceptT.run do
    let (lhs, rhs) <- term.assert2Children
    let lhs <- lhs.toSyntax
    let rhs <- rhs.toSyntax
    op lhs rhs

    

#eval (Node.mk "2" []).toSyntax.run default
#eval (Node.mk "+" []).toSyntax.run default
#eval (Node.mk "+" [Node.mk "1" [], Node.mk "2" []]).toSyntax.run default
#eval (Node.mk "new-set" [Node.mk "1" [], Node.mk "2" []]).toSyntax default
#eval (Node.mk "new-set" [Node.mk "1" [], Node.mk "2" []]).toSyntax default
#eval (Node.mk "union" [Node.mk "1" [], Node.mk "2" []]).toSyntax default


open Lean Meta Elab Term Command in
def doAssumption (a : Assumption) : AnalysisReaderT TermElabM Unit := do
  match a.toAssumptionKind with
  | .error m =>
    logInfo $ m!"Error: {m}"
  | .ok (.eq name value) =>
    let exprSyntax <- match <- value.toSyntax with
      | .error e => throwError e
      | .ok x => pure x
    let expr <- elabTermAndSynthesize exprSyntax none
    let type <- inferType expr
    synthesizeSyntheticMVars
    synthesizeSyntheticMVarsNoPostponing
    logInfo $ m!"{name} : {type} := {expr}"
    addDecl $ Lean.Declaration.defnDecl {
      name := name.toName
      levelParams := []
      type := type
      value := expr
      hints := default
      safety := .safe
    }

-- set_option pp.all true
-- set_option pp.rawOnError true
-- set_option trace.Elab.definition true
-- #eval doAssumption ⟨⟨"+", [⟨"1", []⟩, ⟨"2", []⟩]⟩⟩
#eval doAssumption ⟨⟨"=", [⟨"H", []⟩, ⟨"+", [⟨"1", []⟩, ⟨"2", []⟩]⟩]⟩⟩
  |>.run default
#print H


#eval
  doAssumption
    ⟨⟨"=", [
      ⟨"myNum", []⟩,
      ⟨"1", []⟩
    ]⟩⟩
  |>.run default
#eval
  doAssumption
    ⟨⟨"=", [
      ⟨"mySetty", []⟩,
      ⟨"new-set", [⟨"1", []⟩]⟩,
    ]⟩⟩
  |>.run default
#eval
  doAssumption
    ⟨⟨"=", [
      ⟨"mySet", []⟩,
      ⟨"new-set", [⟨"1", []⟩, ⟨"2", []⟩, ⟨"3", []⟩]⟩,
    ]⟩⟩
  |>.run default
#eval
  doAssumption
    ⟨⟨"=", [
      ⟨"yourSet", []⟩,
      ⟨"new-set", [⟨"1", []⟩, ⟨"2", []⟩, ⟨"3", []⟩, ⟨"4", []⟩]⟩,
    ]⟩⟩
  |>.run default
#eval
  doAssumption
    ⟨⟨"=", [
      ⟨"ourSet", []⟩,
      ⟨"new-set", [⟨"1", []⟩, ⟨"2", []⟩, ⟨"3", []⟩, ⟨"4", []⟩, ⟨"5", []⟩]⟩,
    ]⟩⟩
  |>.run default

#eval
  doAssumption
    ⟨⟨"=", [
      ⟨"anotherSet", []⟩,
      ⟨"union", [
        ⟨"new-set", []⟩,
        ⟨"new-set", [⟨"1", []⟩]⟩,
      ]⟩
    ]⟩⟩
  |>.run {
    isFiniteSet := { ⟨"anotherSet", []⟩ },
    mustBeFiniteSet := { ⟨"anotherSet", []⟩ },
  }

#reduce ourSet
