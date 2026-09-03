module

meta import Latex2Lean

opaque x : Nat → String
opaque y : Int
-- define_latex verbose r"$a = x y$"
-- #guard a = a

/--
error:
Error translating formula 'A = (\sum 3 2)': Could not translate (\sum 3 2).
Errors:
function '\sum had too many arguments!
unsupported formula for translation to set: (\sum 3 2)
unsupported formula for translation to finset: (\sum 3 2)
unsupported formula for translation to multi-set: (\sum 3 2)
-/
-- Ehh, close enough.
#guard_msgs in
define_latex verbose r"$A = \sum 3 2$"
