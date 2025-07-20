import Latex2Lean.Latex.Basic
import Latex2Lean.Latex.TextMode
import Latex2Lean.Latex.MathMode
import Latex2Lean.Latex.MathToNode
import Latex2Lean.Latex.TextToAssumptions
import Latex2Lean.Assumption.Basic
import Latex2Lean.Node.Basic


open Lean (TSyntax)


/-!
This file declares the `Latex.toAssumptions` function.

Important to know that the parser does not actually support all of LaTeX. In
fact, it doesn't even support a large subset of it.
-/

/-
Implementation: decided to use Lean's built-in parsing things to implement the
parser. Lean is very good at declaring parsers.
Why make a parser and not use an existing one? There aren't any good LaTeX
parsers, really. In any language, not just Lean. Turns out that parsing LaTeX is
considered a very hard problem, and that in order to parse LaTeX you basically
need to run LaTeX. So that might make you think that you just need to use an
existing LaTeX compiler! None of them are open enough to allow us to look at
anything like the syntax tree. So we just parse a subset, no big deal.
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
def Latex.toAssumptions (latexString : String) : Lean.CoreM (List Assumption) := do
  let env <- Lean.getEnv
  let stx := Lean.Parser.runParserCategory env `latexText latexString
  let stx <- Lean.ofExcept stx
  let stx : LatexText := TSyntax.mk stx
  let assumptions <- stx.toAssumptions
  return assumptions
