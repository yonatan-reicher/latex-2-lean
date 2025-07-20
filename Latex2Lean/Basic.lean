import Latex2Lean.Analysis
import Latex2Lean.Csv
import Latex2Lean.Node
import Latex2Lean.Latex
import Lean
import Mathlib
import Std


open Lean.Elab.Command (CommandElabM)


def defineLatex (latex : String) : CommandElabM Unit := do
  let assumptions <- Latex.toAssumptions latex
  return 2
