import Latex2Lean.Functions


/-!
Defines the macros that are the front-end of the API.
-/


namespace Latex2Lean


macro "define_latex " "file " lit:str : command =>
  `(#eval defineLatex ($lit : System.FilePath))

macro "define_latex " "file " "verbose " lit:str : command =>
  `(#eval defineLatex ($lit : System.FilePath) (verbose := true))

macro "define_latex " lit:str : command =>
  `(#eval defineLatex $lit)
macro "define_latex " "verbose " lit:str : command =>
  `(#eval defineLatex $lit true)
