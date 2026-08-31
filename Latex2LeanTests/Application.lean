module

meta import Latex2Lean

opaque x : Nat → String
opaque y : Int
define_latex verbose r"$a = x y$"
#guard a = a
