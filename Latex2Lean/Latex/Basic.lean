import Lean


open Lean (TSyntax)


declare_syntax_cat latexText -- So hard not calling this `latext`
declare_syntax_cat latexMath

abbrev LatexText := TSyntax `latexText
abbrev LatexMath := TSyntax `latexMath


inductive BadLatex where
  | math (stx : LatexMath)
  deriving Repr, Inhabited, BEq
