import Latex2Lean


-- define_latex file "proof-adjusted.md"

-- #eval defineLatexFromFile "proof-adjusted.md"
define_latex r##"
# Proof for yuval's Domino Proof

Puzzle: Can you cover a $10 \times 10$ board with 2 by 1 tiles, such that the
number of horizontal and vertical tiles is the same?

This file explains Filmus's solution and proof to the puzzle, "pen and paper"
style. Spoiler: _You can't!_

---

### Definitions
$$
H = \set{ \set{ \set{x, y}, \set{x + 1, y} } \mid 1 }
$$
"##
