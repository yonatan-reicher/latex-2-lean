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
  /-- Can this be used in a binder? -/
  binder : Bool
  deriving DecidableEq, Repr


def TableEntry.ofString (line : String.Slice) : Except String TableEntry := do
  let args :=
    line.split Char.isWhitespace
    |>.toArray.map String.Slice.copy
    |>.filter (not ·.isEmpty)
  let #[name, symbol, predicative, binder] := args
    | throw s!"bad number of arguments, expected 3, got {args.size}"
  let predicative ← do
    match predicative with
    | "predicative" => pure true
    | "non-predicative" => pure false
    | _ => throw s!"expected either 'predicative' or 'non-predicative' at third field"
  let binder ← do
    match binder with
    | "binder" => pure true
    | "non-binder" => pure false
    | _ => throw s!"expected either 'predicative' or 'non-predicative' at third field"
  return { name, symbol, predicative, binder : TableEntry }


def table? : Except String (Array TableEntry) :=
  parse text
where
  text := include_str "../binary_operators.table"
  parse (text : String) : Except String (Array TableEntry) :=
    text.lines
    |>.toArray
    |>.filter fineLine
    |>.mapM TableEntry.ofString
    |> λ | .ok x => pure x
         | .error e => throw s!"binary operator table parsing error: {e}"
  fineLine (s : String.Slice) := not s.isEmpty && not (s.startsWith '#')


#guard_msgs in
#eval
  if let .error e := table? then
    IO.println e
  else pure ()


def table : Array TableEntry :=
  match table? with
  | .ok t => t
  | .error _ => panic! "we just checked this above"


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

    def $(nameDot `binder) : $name → Bool
      $(← cases TableEntry.binder):matchAlt*

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
