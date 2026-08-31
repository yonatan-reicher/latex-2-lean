module

meta import Latex2Lean
meta import Mathlib.Data.Set.Basic

meta def N : Set Nat := .univ
define_latex r"let $\forall x \in N, 200 = x$"
example : ∀ x < 10, 200 = x := by
  intro x h
  have : x ∈ N := by trivial
  exact axiom_4 x this
