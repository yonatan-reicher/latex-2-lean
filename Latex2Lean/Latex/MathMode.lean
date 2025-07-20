import Latex2Lean.Latex.Basic


/-!
This file defines the syntax category for LaTeX math text.
On backslashes: note that because LaTeX uses a lot of '\', we are declaring the
syntax categories with raw string literals, which are strings that do not use
the backslach '\' symbol for escaping.
-/


declare_syntax_cat latexMathBinOp
syntax r" \cap " : latexMathBinOp
syntax r" \cup " : latexMathBinOp
syntax r" + " : latexMathBinOp
syntax r" - " : latexMathBinOp
syntax r" = " : latexMathBinOp
syntax num : latexMath

syntax ident : latexMath
syntax "{" latexMath "}" : latexMath
syntax r"\abs " latexMath : latexMath
syntax r"\{ " latexMath,* r" \}" : latexMath
syntax r"\{ " ident r" \in " latexMath r" \mid " latexMath r" \}" : latexMath
syntax r"\{ " latexMath r" \mid " latexMath r" \}" : latexMath
syntax latexMath latexMathBinOp latexMath : latexMath
macro r"\emptyset" : latexMath => `(latexMath| \{\} )
macro r"\varnothing" : latexMath => `(latexMath| \{\} )
