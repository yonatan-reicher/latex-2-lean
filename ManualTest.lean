import Latex2Lean


-- define_latex file "proof-adjusted.md"
define_latex r"ar"

define_latex r"$X = \set{ (1, 2) }$"
#print X

def y := 2
define_latex file "proof-adjusted.md"

-- #eval defineLatex "This is some text with inline math $x = 2$."
define_latex verbose r"

This is some text with inline math $x = \set{ 1, 2 }$, $\abs x$.
$$
  Y = \set{ \set{ 1, 2 } }
$$

"
#print x
#print Y

define_latex "This is some text with inline math $(x + y) = z$."
