# Proof for yuval's Domino Proof

Puzzle: Can you cover a $10 \times 10$ board with 2 by 1 tiles, such that the
number of horizontal and vertical tiles is the same?

This file explains Filmus's solution and proof to the puzzle, "pen and paper"
style. Spoiler: _You can't!_

---

### Definitions
$H = \set{ \set{ (x, y), (x + 1, y) } \mid x \in 0..8, y \in 0..9 }$
$V = \set{ \set{ (x, y), (x, y + 1) } \mid x \in 0..9, y \in 0..8 }$
$D \subseteq (H \cup V)$

### Assumption
Assume $D$ is a cover of our $10 \times 10$ board:
($in^2$ is the relation $a \in^2 c \iff \exists b \in c, a \in b$)
