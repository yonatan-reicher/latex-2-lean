import Latex2Lean.Util
import Latex2Lean.Input
import Latex2Lean.Spanning
import Latex2Lean.Lexing
import Latex2Lean.Parsing
import Latex2Lean.Categorizing
import Latex2Lean.Analysing
import Latex2Lean.Translating
import Latex2Lean.Emitting


namespace Latex2Lean

open Lean.Elab.Term


def defineLatex {I} [Input I] (inp : I) (verbose : Bool := false)
: TermElabM Unit := do
  -- 1. Read the input
  let input ← Input.getInput inp
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
  -- 8. Emit the Lean commands
  if verbose then
    (← commands.mapM (·.prettyPrint))
    |>.toList
    |> "\n".intercalate
    |> (m!"Emitted commands: \n{·}")
    |> Lean.logInfo
  commands.forM emit

end Latex2Lean
