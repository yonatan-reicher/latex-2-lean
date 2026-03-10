import Latex2Lean
import Mathlib

define_latex r"Let $X = \sum \mset{ 1, 2, 3 }$"
example : X = 6 := by decide

define_latex r"Let $x = \sum \mset{ 3 \mid x \in 1..10 }$"
#print x
example : x = 30 := by decide

def s : Multiset Nat := {1, 2, 1}
def s' := s.pmap (p := fun _ => True) (fun x h => x + 1) (by trivial)
def y := s'.sum
#reduce s'
#print s'
#reduce y
#print y

