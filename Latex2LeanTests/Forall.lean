import Latex2Lean

def N : Set Nat := .univ
define_latex r"let $\forall x \in N, 200 = x$"
example : ∀ x < 10, 200 = x := by
  intro x h
  have : x ∈ N := by trivial
  exact _root_.h x this
#guard h = h -- Check that h exists
