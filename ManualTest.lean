import Latex2Lean


-- define_latex file "proof-adjusted.md"

-- #eval defineLatexFromFile "proof-adjusted.md"
define_latex r"$X = \{ x + 1 \mid x \in \set{ 1, 2, 3 } \}$"
#print X

def y := 2
define_latex r##"
# Proof for yuval's Domino Proof

Puzzle: Can you cover a $10 \times 10$ board with 2 by 1 tiles, such that the
number of horizontal and vertical tiles is the same?

This file explains Filmus's solution and proof to the puzzle, "pen and paper"
style. Spoiler: _You can't!_

---

### Definitions
$H = \set{ \set{ \set{x, y}, \set{x + 1, y} } \mid x \in \set{0,8} }$  
$V = \set{ \set{ \set{x, y}, \set{x, y + 1} } \mid x \in \set{0,9} }$
$D = H \cup V$
"##
