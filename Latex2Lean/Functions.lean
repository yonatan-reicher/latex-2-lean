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

open Lean (logInfo)
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
    |>.mapError (fun e => m!"Error during parsing: {e}")
    |> Lean.ofExcept
  -- 5. Categorize the formulas
  let categorizedFormulas := formulas.map (Prod.map id categorize)
  -- 6. Analyze
  let analysis ← analyze (categorizedFormulas.map Prod.snd)
  -- 7+8. Translate and immediately emit each command so that each definition
  -- is in the environment before the next formula is translated.
  for (_, cf) in categorizedFormulas do
    let some cmd ← translate cf analysis | continue
    if verbose then logInfo (← cmd.prettyPrint)
    emit cmd

end Latex2Lean
