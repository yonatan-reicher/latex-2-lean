import Latex2Lean.Analysis.Basic
import Latex2Lean.Csv
import Latex2Lean.Node.FromString


namespace Latex2Lean


/-!
Parsing the analysis information from a Csv file.
-/


private def Csv.toSet (csv : Csv) : Except String $ Std.HashSet Node := do
  if csv.rows.isEmpty then return ∅
  if h : csv.n != 1 then throw s!"Csv file {csv.fileName} should have exactly one column, but has {csv.n} columns"
  else
    .ofArray <$> csv.rows.mapM fun row => 
      let nodeStr := row[0]'(by grind only)
      Node.fromString nodeStr


/--
Reads the analysis results in the form of Csv files as outputed by our Souffle
code.
-/
def AnalysisResult.fromCsvs (data : List Csv)
: Except String Analysis := do
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

  return {
    isFiniteSet := <- isFiniteSetCsv.toSet
    mustBeFiniteSet := <- mustBeFiniteSetCsv.toSet
  }
