module

meta import Latex2Lean
meta import Mathlib

define_latex r"Let $X = \sum \mset{ 1, 2, 3 }$"
example : X = 6 := by decide

define_latex r"Let $x = \sum \mset{ 3 \mid x \in 1..10 }$"
#print x
example : x = 30 := by decide

meta def s : Multiset Nat := {1, 2, 1}
meta def s' := s.pmap (p := fun _ => True) (fun x h => x + 1) (by trivial)
meta def y := s'.sum
#guard s' = {2, 3, 2}
#print s'
#guard y = 7
#print y
