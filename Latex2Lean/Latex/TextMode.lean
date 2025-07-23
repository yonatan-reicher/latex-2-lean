import Latex2Lean.Latex.Basic
import Lean


/-
We need a parser `notMath` that basically parses any kind of token. Tried:
def notMath := Lean.Parser.notSymbol "$"
This definition does not work. As it turns out, Lean.Parser.notSymbol does not
consume anything, it just checks that the next tokens is not a symbol. We need a
parser that actually consumes anything that isn't the start of math mode.
For now, the following definition will do:
-/
syntax notMath := ident

declare_syntax_cat latexTextAtom
syntax " $ " latexMath " $ " : latexTextAtom
-- TODO: There is a way to make this pretty-print with newlines
syntax " $$ " latexMath " $$ " : latexTextAtom
syntax notMath : latexTextAtom

syntax latexTextAtom* : latexText
