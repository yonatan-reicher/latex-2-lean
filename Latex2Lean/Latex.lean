import Latex2Lean.Latex.Basic
import Latex2Lean.Latex.TextMode
import Latex2Lean.Latex.MathMode
import Latex2Lean.Latex.MathToNode
import Latex2Lean.Latex.TextToAssumptions
import Latex2Lean.Assumption.Basic
import Latex2Lean.Node.Basic


open Lean (TSyntax)


/-!
This file declares the `Latex.mathToNode` and `Latex.toAssumptions` functions.

Important to know that the parser does not actually support all of LaTeX. In
fact, it doesn't even support a large subset of it.
-/

/--
Takes a string of LaTeX in math mode and tries to return an appropriate node.
-/
def Latex.mathToNode (latexString : String) : Lean.CoreM Node := do
  let env <- Lean.getEnv
  let stx := Lean.Parser.runParserCategory env `latexMath latexString
  let stx <- Lean.ofExcept stx
  let stx : LatexMath := TSyntax.mk stx
  let node <- stx.toNode
  return node

#eval do Latex.mathToNode r"\{ 1, 2, \{ 1, 2 \} \}"


/--
Takes some LaTeX code in text mode and returns all the "assumptions" that could
be read from it in a list.
-/
def Latex.toAssumptions (latexString : String)
: Lean.CoreM (List Assumption × List Node) := do
  let env <- Lean.getEnv
  let stx := Lean.Parser.runParserCategory env `latexText latexString
  let stx <- Lean.ofExcept stx
  let stx : LatexText := TSyntax.mk stx
  let assumptions <- stx.toAssumptions
  let nodes <- stx.toNodes
  return (assumptions, nodes)
