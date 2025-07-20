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
  let assumptions <- liftCoreM $ Latex.toAssumptions latex
  let assumptionsCsv : Csv := {
    fileName := "assumptions.csv"
    rows := assumptions.toArray.map (Vector.singleton ·.toString)
    n := _
  }
  let outputs <- liftIO $ Souffle.call #[assumptionsCsv] (wsl := false)
  let analysisResult <- Lean.ofExcept $ AnalysisResult.fromCsvs outputs.toList
  for a in assumptions do
    let c <- liftCoreM $ a.toCommand.run analysisResult
    Lean.logInfo c
    elabCommand c
