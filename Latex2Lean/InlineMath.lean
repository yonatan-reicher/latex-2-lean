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

def parseFromPrefix : Array Char → Option Kind
  | array =>
    if array[:2] == #['$', '$'] then .some .doubleDollar
    else if array[:1] == #['$'] then .some .singleDollar
    else .none

end Kind


structure Span where
  leftDelimStart : Nat
  rightDelimStart : Nat
  start : Nat
  text : Array Char
  deriving Repr
