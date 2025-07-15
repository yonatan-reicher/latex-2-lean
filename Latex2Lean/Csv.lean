import Latex2Lean.Parentheses


/-- Represents names of files -/
def FileName := String
  deriving Repr, DecidableEq, Inhabited, Hashable


/-- A structure of the contents of some CSV file. -/
structure Csv where
  {n : Nat}
  fileName : FileName
  columns : Vector String n
  rows : Array (Vector String n)
  deriving Repr, DecidableEq, Inhabited


namespace Csv


def read : FileName -> List String -> Except String Csv
  | _, [] => .error "No columns"
  | fileName, header :: rest => do
    let columns := (splitRow header).toVector
    let n := columns.size
    let mut rows := #[]
    let mut i := 0
    for row in rest do
      i := i + 1
      let row := splitRow row
      if h : n = row.size then
        rows := rows.push (show Vector _ n from h ▸ row.toVector)
      else
        throw s!"CSV has {n} columns, but row {i} has {row.size}"
    return { fileName := fileName, columns := columns, rows := rows }
  where
    splitRow : String -> Array String
    | row =>
      row.splitButParens ','
      |>.map (·.toString.trim)

#guard
  read "Hello,World" ["[ hello, world ], 2"]
  |> fun x => match x with
  | .ok { n := 2, .. } => true
  | _ => false
