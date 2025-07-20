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
  let (assumptions, nodes) <- liftCoreM $ Latex.toAssumptions latex
  let outputs <- liftIO $ Souffle.call (wsl := false) #[
    { fileName := "assumption.csv"
      rows := assumptions.toArray.map (Vector.singleton ·.toString)
      n := _ },
    { fileName := "expr.csv"
      rows := nodes.toArray.map (Vector.singleton ·.toString)
      n := _ },
  ]
  let analysisResult <- Lean.ofExcept $ AnalysisResult.fromCsvs outputs.toList
  for a in assumptions do
    let c <- liftCoreM $ a.toCommand.run analysisResult
    Lean.logInfo c
    elabCommand c


#eval defineLatex r"
This is a file about $ H = \{1, 2, 3\} $ hello!
"

#print H

