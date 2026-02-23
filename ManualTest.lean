import Latex2Lean
import Mathlib


def y := 2
define_latex file verbose "proof-adjusted.md"

define_latex verbose r"
$$
X = \{ x + 1 \mid x \in \{1, 3\} \}
$$
hello! $a = \abs X$
"
#print X
#print a
