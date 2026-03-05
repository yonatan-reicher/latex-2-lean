import Latex2Lean
import Mathlib


axiom D {α} : Set α
define_latex verbose r"X \subseteq \emptyset"
define_latex file verbose "proof-adjusted.md"

define_latex verbose r"
$$
X = \{ x + 1 \mid x \in \{1, 3\} \}
$$
$$
Y = \mset{ 1, 2, 5, 2 }
$$
hello! $a = \abs X$
"
#print X
#print Y
#print a
