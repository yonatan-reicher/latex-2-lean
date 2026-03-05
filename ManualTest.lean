import Latex2Lean
import Mathlib


axiom D {α} : Set α
define_latex file verbose "proof-adjusted.md"


define_latex verbose r"

Let $x = \sum \mset{ 1 }$


"
example : x = 1 := by decide
