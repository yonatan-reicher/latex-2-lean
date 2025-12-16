import Latex2Lean.Functions


/-!
Defines the macros that are the front-end of the API.
-/


namespace Latex2Lean


macro "define_latex " "file " lit:str : command => `(#eval defineLatexFromFile $lit)

macro "define_latex " lit:str : command => `(#eval defineLatex $lit)
