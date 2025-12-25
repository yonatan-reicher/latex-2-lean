import Latex2Lean.Pos


namespace Latex2Lean.InlineMath


/-- One of the kinds of inline maths we support. -/
inductive Kind : Type
  | singleDollar
  | doubleDollar
  deriving DecidableEq


namespace Kind

def toString : Kind → String
  | singleDollar => "$"
  | doubleDollar => "$$"

instance : ToString Kind where
  toString := Kind.toString

def all : Array Kind :=
  #[
    .singleDollar,
    .doubleDollar,
  ]

def parseFromPrefix : Subarray Char → Option Kind
  | array =>
    if array[:2].toArray == #['$', '$'] then .some .doubleDollar
    else if array[:1].toArray == #['$'] then .some .singleDollar
    else .none

end Kind


structure Span where
  leftDelimStart : Pos
  rightDelimStart : Pos
  start : Pos
  text : Array Char
  deriving Repr
