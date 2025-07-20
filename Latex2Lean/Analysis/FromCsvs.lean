import Latex2Lean.Analysis.Basic
import Latex2Lean.Csv
import Latex2Lean.Node.FromString


private def decidable (p : Prop) [Decidable p] : Decidable p := inferInstance


/--
Reads the analysis results in the form of Csv files as outputed by our Souffle
code.
-/
def AnalysisResult.fromCsvs (data : List Csv)
: Except String Analysis := do
  dbg_trace repr data

  let csvNames :=
    data.map (·.fileName)
    -- WHY would someone name this INTERCALATE??
    |> String.intercalate ", "
  let isFiniteSetCsvName := "expr_is_finite_set.csv"
  let mustBeFiniteSetCsvName := "expr_must_be_finite_set.csv"

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
    |> Std.HashSet.ofArray
  let mustBeFiniteSet :=
    (<- mustBeFiniteSetCsv.rows.mapM fun row =>
      let nodeStr := row[0]'(by grind only)
      Node.fromString nodeStr
    )
    |> Std.HashSet.ofArray

  return {
    isFiniteSet := isFiniteSet
    mustBeFiniteSet := mustBeFiniteSet
  }
