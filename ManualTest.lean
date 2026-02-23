import Latex2Lean


def y := 2
define_latex file verbose "proof-adjusted.md"

define_latex verbose r"
$$
X = \{ x + 1 \mid x \in \{1, 3\} \}
$$
"
#print X
