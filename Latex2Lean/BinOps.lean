module

import Lean
meta import Lean

open Lean (Name TSyntax mkIdent quote)
open Lean.Elab.Command (elabCommand)
open Lean.Parser.Command (ctor)
open Lean.Syntax (mkStrLit)
abbrev Ctor := TSyntax ``ctor

namespace Latex2Lean


-- =================================================================================================
--                                       Binary Operator Table
-- =================================================================================================


structure TableEntry where
  name : String
  symbol : String
  predicative : Bool
  deriving DecidableEq


def TableEntry.ofString (line : String.Slice) : Except String TableEntry := do
  let args :=
    line.split Char.isWhitespace
    |>.toArray.map String.Slice.copy
    |>.filter (not ·.isEmpty)
  let #[name, symbol, predicative] := args
    | throw s!"bad number of arguments, expected 3, got {args.size}"
  let predicative ← do
    match predicative with
    | "predicative" => pure true
    | "non-predicative" => pure false
    | _ => throw s!"expected either 'predicative' or 'non-predicative' at third field"
  return { name, symbol, predicative : TableEntry }


def table : Array TableEntry :=
  let table_text := by_elab
    let table_text ← IO.FS.readFile "binary_operators.table"
    return Lean.mkStrLit table_text
  table_text.lines.toArray
  |>.filter fineLine
  |>.mapM TableEntry.ofString
  |> λ | .ok x => x
       | .error e => panic! s!"binary operator table parsing error: {e}"
where
  fineLine (s : String.Slice) := not s.isEmpty && not (s.startsWith '#')


-- =================================================================================================
--                                          Defining Types
-- =================================================================================================


def defineBinOpType := do
  elabCommand =<< `(command|
    inductive $name where
      $(← ctors)*
    deriving DecidableEq, Inhabited, Repr
  )
where
  -- For some reason, if we don't put this in a variable Lean fails??
  name := Lean.mkIdent `BinOp
  ctors := table.mapM λ entry =>
    `(ctor| | $(mkIdent $ Name.mkSimple entry.name):ident)


public section
run_cmd defineBinOpType
end
