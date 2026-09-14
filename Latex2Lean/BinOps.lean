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

local macro "bin_op" name:ident symbol:term pred:("predicative")? : term => do
  `({
    name := $(name.getId.toString |> mkStrLit):term
    symbol := $symbol
    predicative := $(quote pred.isSome)
    : TableEntry
  })

def table := #[
  -- logic
  bin_op  eq         "="          predicative,
  bin_op  land      r"\land"      predicative,
  bin_op  lor       r"\lor"       predicative,
  -- arithmetic
  bin_op  ge        r"\ge"        predicative,
  bin_op  gt         ">"          predicative,
  bin_op  le        r"\le"        predicative,
  bin_op  lt         "<"          predicative,
  bin_op  minus      "-",
  bin_op  plus       "+",
  bin_op  slash      "/",
  bin_op  star       "*",
  -- set operations
  bin_op  cap       r"\cap",
  bin_op  cup       r"\cup",
  bin_op  in_       r"\in"        predicative,
  bin_op  subset    r"\subset"    predicative,
  bin_op  subseteq  r"\subseteq"  predicative,
  bin_op  supset    r"\supset"    predicative,
  bin_op  supseteq  r"\supseteq"  predicative,
  bin_op  times     r"\times",
]

-- ------ Define the BinOp type --------------------------------------------------------------------

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
