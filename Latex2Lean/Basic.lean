import Latex2Lean.Analysis
import Latex2Lean.Assumption
import Latex2Lean.Csv
import Latex2Lean.Latex
import Latex2Lean.Node
import Latex2Lean.Souffle
import Lean
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
  let (assumptions, nodes) <- match Latex.textToAssumptions latex with
    | Except.ok (as, ns) => pure (as, ns)
    | Except.error e => throwError m!"Error: {repr e}"
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


def defineLatexFromFile (fileName : System.FilePath) : CommandElabM Unit := do
  let s <- IO.FS.readFile fileName
  defineLatex s


macro "define_latex " "file " lit:str : command => `(#eval defineLatex $lit)

macro "define_latex " lit:str : command => `(#eval defineLatex $lit)


