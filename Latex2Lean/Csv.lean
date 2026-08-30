module

public import Latex2Lean.Util
public import Std.Data.HashSet.Basic

public section



namespace Latex2Lean


open System ( FilePath )


/-- A structure of the contents of some Csv file. -/
structure Csv where
  {n : Nat}
  fileName : FilePath
  rows : Array (Vector String n)
  deriving Repr, DecidableEq, Inhabited


namespace Csv


def read (path : FilePath) (text : String) : Except String Csv := do
  let rows := text.splitOn "\n"
  -- Ignore last empty line
  let rows := if not rows.isEmpty && rows.getLast!.length == 0 then rows.dropLast else rows
  if rows == [] then
    -- throw s!"empty csv file '{fileName}'"
    return { n := 0, fileName := path, rows := #[] }
  let rows := rows.toArray.map splitRow
  let sizes := rows.map (·.size) |> Std.HashSet.ofArray |>.toList
  if sizes.isEmpty then panic! "this list should not be empty"
  let [n] := sizes
    | throw s!"csv rows in a file should have the same count, but file '{path}' contains counts {sizes}"
  let rows := rows.map λ row =>
    if h : row.size = n then h ▸ row.toVector
    else panic!"all csv rows should have the same size"
  return { fileName := path, rows := rows, n := n }
where
  splitRow : String -> Array String
  | row =>
    row.splitButParens ',' '[' ']'
    |>.map (·.toString.trimAscii.copy)

#guard
  read "Foo" "[ hello, world ], 2"
  |> fun x => match x with
  | .ok { rows := #[ vec ], .. } => vec.toArray = #["[ hello, world ]", "2"]
  | _ => false


/-- Note: ignores file name... obviously -/
def toLines (csv : Csv) (delim := ", ") : Array String :=
  csv.rows.map λ row => row.toList |> delim.intercalate
