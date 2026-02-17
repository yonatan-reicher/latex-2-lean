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
import Latex2Lean.Translating
import Latex2Lean.Emitting


namespace Latex2Lean

open Lean.Elab.Command (CommandElabM liftTermElabM)


def defineLatex (text : String) : CommandElabM Unit := do
  -- 1. Read the input
  let input := Input.str text |> Array.toSubarray
  -- 2. Span the input into math spans
  let spans ← span input
    |>.mapError (fun e => m!"Error during spanning: {repr e}")
    |> Lean.ofExcept
  -- 3. Lex the math spans into tokens
  let tokens : Array (InlineMath.Kind × Array Token) := spans
    |>.map fun (kind, s) => (kind, lex s.text s.start)
  -- 4. Parse the tokens into formulas
  let formulas : Array (InlineMath.Kind × Formula) ← tokens
    |>.mapM (fun (kind, t) => return (kind, ← parse kind (t : Array Token)))
    |>.mapError (fun e => m!"Error during parsing: {repr e}")
    |> Lean.ofExcept
  -- 5. Categorize the formulas
  let categorizedFormulas := formulas.map (Prod.map id categorize)
  -- 6. Analyze
  let analysis ← analyze (categorizedFormulas.map Prod.snd)
  -- 7. Translate to Lean commands
  let commands : Array LeanCmd ← categorizedFormulas
    |>.filterMapM (translate ·.2 analysis)
    |> liftTermElabM
  -- 8. Emit the Lean commands
  commands.forM emit

#eval defineLatex "This is some text with inline math $x = \\set{2}$. $\\abs x$"
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
