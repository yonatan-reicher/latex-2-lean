-- import Latex2Lean.Analysis
-- import Latex2Lean.Assumption
-- import Latex2Lean.Csv
-- import Latex2Lean.Latex
-- import Latex2Lean.Node
-- import Latex2Lean.Souffle
import Latex2Lean.Input
import Latex2Lean.Spanning
import Latex2Lean.Lexing
import Latex2Lean.Parsing
-- import Latex2Lean.Categorizing
-- import Latex2Lean.Analysing
-- import Latex2Lean.Translating
-- import Latex2Lean.Emitting
import Lean
import Std


open Lean.Elab.Command (
  CommandElabM
  elabCommand
  liftCoreM
  liftIO
)


namespace Latex2Lean


def defineLatex text :=
  text
  |> Input.str
  |> Array.toSubarray
  |> span
  |> (fun e =>
    match e with
    | Except.ok e => e
    | .error e => panic! ""
  )
  |> Array.map (fun (inlineMathKind, s) => (inlineMathKind, lex s.text.toSubarray s.start))
  |> Array.map (fun (inlineMathKind, tokens) => parse inlineMathKind tokens.toSubarray)


/-
def defineLatex (latex : String) : CommandElabM Unit := do
  -- Read the assumptions from the input
                                            -- TODO rename this function huh?
  let (assumptions, nodes) <- match Latex.textToAssumptions latex with
    | .ok (as, ns) => pure (as, ns)
    | .error e => throwError m!"Error: {repr e}"
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
    elabCommand c


def defineLatexFromFile (fileName : System.FilePath) : CommandElabM Unit := do
  let s <- IO.FS.readFile fileName
  defineLatex s
-/
