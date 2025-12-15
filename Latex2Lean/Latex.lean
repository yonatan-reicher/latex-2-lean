import Latex2Lean.Latex.Basic
import Latex2Lean.Latex.TextMode
import Latex2Lean.Latex.MathMode
import Latex2Lean.Latex.TextToAssumptions
import Latex2Lean.Assumption.Basic
import Latex2Lean.Node.Basic


open Lean (TSyntax)


namespace Latex


/-!
This file declares the `Latex.mathToNode` and `Latex.textToAssumptions`
functions.

Important to know that the parser does not actually support all of LaTeX. In
fact, it doesn't even support a large subset of it.
-/


def M := Except Error
abbrev MAbbrev := Except Error
instance : Monad M := inferInstanceAs $ Monad MAbbrev
instance : MonadExcept Error M := inferInstanceAs $ MonadExcept Error MAbbrev
instance {m} [Monad m] [Lean.MonadError m] : MonadLift M m where
  monadLift a := Lean.ofExcept a

/--
Takes a string of LaTeX in math mode and tries to return an appropriate node.

The language is documented at Latex2Lean/Latex/MathMode.lean.
-/
def mathToNode (latexString : String) : M Node := do
  match Latex.mathModeForSure.parse $ .ofString latexString with
  | .ok x _ => return x
  | .fail f _ => nomatch f
  | .error e _ => throw e

#eval do Latex.mathToNode r"\{ 1, 2, \{ 1, 2 \} \}"

#eval do Latex.textToNodes r"
Hello mister $N = 42$ eyes.
"
