import Latex2Lean


define_latex r"Let $x = \sum \mset{ 1, 2, 3 }$"
example : x = 6 := by decide

define_latex r"Let $x = \sum \mset{ 3 \mid x \in 1..10 }$"
example : x = 30 := by decide
