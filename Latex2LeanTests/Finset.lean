module

meta import Latex2Lean

meta def X : Finset Bool := .univ
meta def Y : Finset Bool := .univ
meta def Z : Finset Bool := .univ
define_latex verbose r"
  let $W = \set{ x \mid x \in X, y \in Y }$
  $a = \abs W$
"

#print W
#print a
