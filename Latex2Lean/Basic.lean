import Latex2Lean.Analysis
import Latex2Lean.Assumption
import Latex2Lean.Csv
import Latex2Lean.Latex
import Latex2Lean.Node
import Latex2Lean.Souffle
import Lean
import Mathlib
import Std


open Lean.Elab.Command (
  CommandElabM
  elabCommand
  liftCoreM
  liftIO
)


def defineLatex (latex : String) : CommandElabM Unit := do
  -- Read the assumptions from the input
                                            -- TODO rename this function huh?
  let (assumptions, nodes) := match Latex.textToAssumptions latex with
    | Except.ok (as, ns) => (as, ns)
    | Except.error e => panic! s!"Error: {repr e}"
  let outputs <- liftIO $ Souffle.call (wsl := false) #[
    { fileName := "assumption.csv"
      rows := assumptions.map (Vector.singleton ·.toString)
      n := _ },
    { fileName := "expr.csv"
      rows := nodes.map (Vector.singleton ·.toString)
      n := _ },
  ]
  let analysisResult <- Lean.ofExcept $ AnalysisResult.fromCsvs outputs.toList
  for a in assumptions do
    let c <- liftCoreM $ a.toCommand.run analysisResult
    Lean.logInfo c
    elabCommand c


#eval defineLatex r"
Let $ H = \{1, 2\}$. Can we know use it in Lean?
$ \abs H $
"

#print H


#eval defineLatex r##"
# Proof for Yuval's Domino Proof

Puzzle: Can you cover a $10 \times 10$ board with 2 by 1 tiles, such that the
number of horizontal and vertical tiles is the same?

This file explains Filmus's solution and proof to the puzzle, "pen and paper"
style. Spoiler: _You can't!_

<!-- Here are some useful latex shortcuts for our use: -->
$$
\newcommand{\tup}[1]{\left\langle #1 \right\rangle}
\newcommand{\abs}[1]{\left| #1 \right|}
$$

---

### Definitions
$$
H = \set{ \set{ \tup{x, y}, \tup{x + 1, y} } \mid \substack{ x \in 0..8 \\ y \in 0..9 } } \\
V = \set{ \set{ \tup{x, y}, \tup{x, y + 1} } \mid \substack{ x \in 0..9 \\ y \in 0..8 } } \\
D \subseteq H \cup V
$$

### Assumptions
Assume $D$ is a cover of our $10 \times 10$ board:  
($\in^2$ is the relation $a \in^2 c \iff \exists b \in c, a \in b$)
$$
\begin{align}
& \forall d_1, d_2 \in D.~ d_1 \cap d_2 = \varnothing \\
& \set{p \mid p \in^2 D} = 0..9 \times 0..9
\end{align}
$$

And assume, by contradiction, $\abs{D \cap H} = \abs{D \cap V}$ 

### Conjectures
| Proposition                                                   | Explanation |
|-------------                                                  |-------------|
| $ \abs D = 50$                                                | Our board's size is 100, and every element is a set of size 2, hence: 50 tiles |
| $(D \cap H) \cup (D \cap V) = D$                              | By $D \subseteq H \cup V$ |
| $(D \cap H) \cap (D \cap V) = \varnothing$                    | Because $H$ and $V$ are disjoint! |
| $\abs{D \cap H} = \abs{D \cap V} = 25$                        | By the assumption, and by previous two conjuctures |
| $\sum \set{ x \mid \tup{x, y} \in^{2} D \cap V }$ is even     | Every number in the sum appears twice, as the $x$'s of a tile's cells are equal!
| $\sum \set{ x \mid \tup{x, y} \in^{2} D \cap H }$ is odd      | Here every number appears with it's successor by similar reasoning
| $\sum \set{ x \mid \tup{x, y} \in 0..9 \times 0..9 }$ is even | By decide |

Now we notice
$$
\begin{align*}
& \sum \set{ x \mid \tup{x, y} \in 0..9 \times 0..9 } \\
& = \sum \set{ x \mid \tup{x, y} \in^{2} D \cap V }
  + \sum \set{ x \mid \tup{x, y} \in^{2} D \cap H }
\end{align*}
$$

By the conjectures above, we have a contradiction! (A number can't be both even
and odd)
"##

