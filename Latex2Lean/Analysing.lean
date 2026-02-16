import Latex2Lean.CategorizedFormula
import Latex2Lean.Analysis
import Latex2Lean.Souffle


/-!
Defines the analysis stage, which takes the categorized formulas and runs the
static analysis defined by the Souffle code on them. In order to send the
assumptions to the static analysis, we actually need to send them as strings in
CSV files, in a format we refer to as "nodes".
-/


namespace Latex2Lean


private def csvs (formulas : Subarray CategorizedFormula) : Array Csv := Id.run do
  let mut assumptions := #[]
  let mut expressions := #[]
  for f in formulas do
    let isAssumption := match f with
      | .definition .. | .axiom_ .. => true
      | .plain .. => false
    -- let row := #[f.toFormula.toNode.toString].toVector
    -- if isAssumption
    -- then assumptions := assumptions.push row
    -- else expressions := expressions.push row
  return #[
    Csv.mk (n:=1) "assumptions.csv" assumptions,
    Csv.mk (n:=1) "expressions.csv" expressions,
  ]


def analyze (formulas : Subarray CategorizedFormula) : IO Analysis := do
  let csvs := csvs formulas
  let result <- Souffle.call csvs (wsl := false)
  default
