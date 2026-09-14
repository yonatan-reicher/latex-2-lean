module

import Lean
meta import Lean

open Lean (Name TSyntax mkIdent quote)
open Lean.Elab.Command (elabCommand)
open Lean.Parser.Command (ctor)
open Lean.Parser.Term (matchAltExpr matchAlts matchAltsWhereDecls)
open Lean.Syntax (TSepArray mkStrLit)
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
  elabCommand =<< `(
    inductive $name where
      $(← ctors)*
    deriving DecidableEq, Inhabited, Repr

    def $(nameDot `name) : $name → String
      $(← cases TableEntry.name):matchAlt*

    def $(nameDot `symbol) : $name → String
      $(← cases TableEntry.symbol):matchAlt*

    def $(nameDot `predicative) : $name → Bool
      $(← cases TableEntry.predicative):matchAlt*

    def $(nameDot `all) : Array $name := $(← all)
  )
where
  -- For some reason, if we don't put this in a variable Lean fails??
  name := mkIdent `BinOp
  identOf e := mkIdent $ Name.mkSimple $ TableEntry.name $ e
  ctors := table.mapM λ e => `(ctor| | $(identOf e):ident)
  nameDot n := mkIdent $ name.getId ++ n
  cases {α} (f : TableEntry → α) [Lean.Quote α] :=
    table.mapM λ e => `(matchAltExpr| | $(identOf e):ident => $(quote $ f e) )
  all := table.map (identOf ·) |> λ x : Array Lean.Term => (``(#[$x,*]))


public section

run_cmd defineBinOpType

theorem BinOp.mem_all : ∀ op, op ∈ all := by
  intro op
  simp only [all, List.mem_toArray, List.mem_cons, List.not_mem_nil, or_false]
  cases op <;> decide

instance : ToString BinOp where
  toString := BinOp.symbol

end
