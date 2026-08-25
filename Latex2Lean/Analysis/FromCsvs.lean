import Latex2Lean.Analysis.Basic
import Latex2Lean.Csv
import Latex2Lean.Node.FromString


namespace Latex2Lean


/-!
Parsing the analysis information from a Csv file.
-/


private def Csv.toSet (csv : Csv) : Except String $ Std.HashSet Formula.Id := do
  if csv.rows.isEmpty then return ∅
  if h : csv.n != 1 then throw s!"Csv file {csv.fileName} should have exactly one column, but has {csv.n} columns"
  else
    .ofArray <$> csv.rows.mapM fun row => 
      let idStr := row[0]'(by grind only)
      match idStr.toNat? with
      | none => throw s!"Bad id in {csv.fileName} ('{idStr}')"
      | some id => .ok id


/--
Reads the analysis results in the form of Csv files as outputed by our Souffle
code.
-/
def Analysis.fromCsvs (data : List Csv)
: Except String Analysis := do
  let csvNames :=
    data.map (·.fileName.toString)
    -- WHY would someone name this INTERCALATE??
    |> String.intercalate ", "
  let isFiniteSetCsvName := "is_finite"
  let mustBeFiniteSetCsvName := "used_as_finite"

  -- Find the Csvs!
  let some isFiniteSetCsv :=
    data.find? (·.fileName.toString == isFiniteSetCsvName)
    | throw s!"No CSV file named '{isFiniteSetCsvName}'. Found only '{csvNames}'"
  let some mustBeFiniteSetCsv :=
    data.find? (·.fileName.toString == mustBeFiniteSetCsvName)
    | throw s!"No CSV file named '{mustBeFiniteSetCsvName}'. Found only '{csvNames}'"

  return {
    isFiniteSet := <- isFiniteSetCsv.toSet
    mustBeFiniteSet := <- mustBeFiniteSetCsv.toSet
  }
