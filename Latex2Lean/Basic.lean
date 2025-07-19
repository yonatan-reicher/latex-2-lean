import Latex2Lean.Analysis
import Latex2Lean.Csv
import Latex2Lean.Node
import Lean
import Mathlib
import Std


open Lean (Syntax TSyntax MetaM CoreM)
open Lean.Elab.Term (TermElabM)
open Lean.Elab.Command (CommandElabM)
open Std (HashSet)


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

/-
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
-/

#reduce ourSet
