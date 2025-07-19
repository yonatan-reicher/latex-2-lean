import Latex2Lean.Parentheses
import Std


/-- Represents names of files -/
abbrev FileName := String


/-- A structure of the contents of some CSV file. -/
structure Csv where
  {n : Nat}
  fileName : FileName
  rows : Array (Vector String n)
  deriving Repr, DecidableEq, Inhabited


namespace Csv


def read : FileName -> List String -> Except String Csv
  | fileName, [] => throw s!"empty csv file '{fileName}'"
  | fileName, rows => do
    let rows := rows.toArray.map splitRow
    let sizes := rows.map (·.size) |> Std.HashSet.ofArray |>.toList
    if sizes.isEmpty then panic! "this list should not be empty"
    let [n] := sizes
      | throw s!"csv rows in a file should have the same count, but file '{fileName}' contains counts {sizes}"
    let rows := rows.map λ row =>
      if h : row.size = n then h ▸ row.toVector
      else panic!"all csv rows should have the same size"
    return { fileName := fileName, rows := rows, n := n }
  where
    splitRow : String -> Array String
    | row =>
      row.splitButParens ','
      |>.map (·.toString.trim)

#guard
  read "Foo" ["[ hello, world ], 2"]
  |> fun x => match x with
  | .ok { rows := #[ vec ], .. } => vec.toArray = #["[ hello, world ]", "2"]
  | _ => false
