import Latex2Lean


-- For some reason, no messages are printed? this is weird, because the given
-- file name does not refer to an actual file! so an exception is definitly
-- thrown inside. It's just not printed??
define_latex file "proof"

-- #eval defineLatexFromFile "proof-adjusted.md"
#eval defineLatex r##"
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
