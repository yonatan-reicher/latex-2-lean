module

meta import Latex2Lean

meta def N : Set Nat := .univ
meta opaque P : Set Nat
define_latex r"let $\exists x \in N, x \in P$"

example : ∃ y ∈ N, y ∈ P := by exact?
