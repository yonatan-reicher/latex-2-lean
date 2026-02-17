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
import Latex2Lean.Categorizing
import Latex2Lean.Analysing
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
  -- 1. Read the input
  |> Input.str
  |> Array.toSubarray
  -- 2. Span the input into math spans
  |> span
  |> (fun e =>
    match e with
    | Except.ok e => e
    | .error e => panic! "aa"
  )
  -- 3. Lex the math spans into tokens
  |> Array.map (fun (inlineMathKind, s) => (inlineMathKind, lex s.text.toSubarray s.start))
  -- 4. Parse the tokens into formulas
  |> Array.mapM (fun (inlineMathKind, tokens) => parse inlineMathKind tokens.toSubarray)
  |> (fun e =>
    match e with
    | Except.ok e => e
    | .error e => panic! "aa"
  )
  -- 5. Categorize the formulas
  |> Array.map categorize
  -- 6. Analyze
  |> analyze

#eval defineLatex "This is some text with inline math $(x + y) = z$."
#eval
  "arstarsta   $x = 2$"
  |> Input.str
  |> Array.toSubarray
  |> span
  |> (fun
    | Except.ok e => e
    | .error e => panic! "aa"
  )

end Latex2Lean


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
