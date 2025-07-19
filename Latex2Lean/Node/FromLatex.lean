import Lean
import Latex2Lean.Node.Basic


/-!
This file declares the `Node.fromLatex` function.

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


open Lean (TSyntax CoreM Expr)


/--
This is the syntax category for LaTeX text.
On backslashes: note that because LaTeX uses a lot of '\', we are declaring the
syntax categories with raw string literals, which are strings that do not use
the backslach '\' symbol for escaping.
-/
declare_syntax_cat latex
declare_syntax_cat latexBinOp
syntax num : latex
syntax ident : latex
syntax r"\{ " latex,* r" \}" : latex
syntax r"\{ " ident r" \in " latex r" \mid " latex r" \}" : latex
syntax r"\{ " latex r" \mid " latex r" \}" : latex
syntax latex latexBinOp latex : latex
macro r"\emptyset" : latex => `(latex| \{\} )
macro r"\varnothing" : latex => `(latex| \{\} )

syntax r" \cap " : latexBinOp
syntax r" \cup " : latexBinOp
syntax r" + " : latexBinOp
syntax r" - " : latexBinOp


/-- Get the unhygenic String of an identifier -/
private def identToString (ident : TSyntax `ident) : String :=
  ident.getId.eraseMacroScopes.toString


/-- Get the string of a number. -/
private def numToString (num : TSyntax `num) : String :=
  toString num.getNat


abbrev BadLatex := TSyntax `latex


instance : MonadLift (Except BadLatex) Lean.CoreM where
  monadLift except :=
    match except with
    | .ok x => return x
    | .error e => do
      let f <- liftM $ Lean.PrettyPrinter.formatCategory `latex e
      throwError s!"The syntax '{f}' is unsupported"


/--
Takes LaTeX syntax as input, and returns the corr
-/
private partial def Node.fromLatexSyntax : TSyntax `latex -> Except BadLatex Node
  | `(latex| $var:ident) => return ⟨identToString var, []⟩
  | `(latex| $n:num) => return ⟨numToString n, []⟩
  | `(latex| \{ $args,* \}) => do
    let nodes <- args.getElems.mapM fromLatexSyntax
    return ⟨"new-set", nodes.toList⟩
  | unknown => throw unknown


#eval show CoreM _ from do Node.fromLatexSyntax (<- `(latex|\{\}))
#eval show CoreM _ from do liftM $ Node.fromLatexSyntax (<- `(latex|124))
#eval show CoreM _ from do liftM $ Node.fromLatexSyntax (<- `(latex|hello))
#eval show CoreM _ from do liftM $ Node.fromLatexSyntax (<- `(latex|hello))
#eval show CoreM _ from do liftM $ Node.fromLatexSyntax (<- `(latex|\{ x \in 1 \mid 1 \}))


/-!
This file is about converting LaTeX code to `Node` values inside lean.

Implementation:
We are using Lean's Meta-Programming features to parse the LaTeX (we can even
use them to parse from a string, not just from source code!
-/

def Node.fromLatex (latexString : String) : Lean.CoreM Node := do
  let env <- Lean.getEnv
  let stx := Lean.Parser.runParserCategory env `latex latexString
  let stx <- Lean.ofExcept stx
  let stx : TSyntax `latex := TSyntax.mk stx
  let node <- Node.fromLatexSyntax stx
  return node

#eval do Node.fromLatex r"\{ 1, 2, \{ 1, 2 \} \}"
