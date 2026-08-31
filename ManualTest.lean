module

public import Latex2Lean
public import Mathlib

public section

axiom D : Set (Set (Nat × Nat))
define_latex file verbose "proof-adjusted.md"

abbrev Segment := Nat × Nat
abbrev Multisegment := List Segment
def m : Multisegment := [(1, 2), (2, 3), (4, 6)]
def Indices (m : Multisegment) : Set (List Nat) :=
  { i : List Nat | ¬i.isEmpty ∧ ∀ x ∈ i, x < m.length }


define_latex verbose r"
  $$
    M = max \set{
      j \mid \exists I ∈ Indices m,
        (m[i[0]] = s)
        ∧ ∀ r < j, segmentLt m[i[r]] m[i[r + 1]]
    }
  $$
"


/-

Given m ∈ List (Nat × Nat), we want the following defintion:

max \set{
  j \mid \exists I ∈ Indices m,
    (m[i[0]] = s)
    ∧ ∀ r < j, segmentLt m[i[r]] m[i[r + 1]]
}


max {
  j : ℕ |
  ∃ i : Indices j m,
    (m.segments[i.val[0]] = s) ∧
    (∀ r, (h_r : r < j) → (m.segments[i.val[r]] ≪ m.segments[i.val[r+1]]))
    -- This one had m.segments[r] instead of m.segments[i[r]], a bug I assume?
}



-/


define_latex verbose r"

Let $x = \sum \mset{ 1 }$


"
example : x = 1 := by decide


define_latex file verbose "ladder.md"
