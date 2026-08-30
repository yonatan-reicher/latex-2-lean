module

public import Latex2Lean
public import Mathlib

public section

axiom D : Set (Set (Nat × Nat))
define_latex file verbose "proof-adjusted.md"


define_latex verbose r"

Let $x = \sum \mset{ 1 }$


"
example : x = 1 := by decide


define_latex file verbose "ladder.md"
