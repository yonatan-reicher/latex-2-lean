import Latex2Lean.Analysis.Basic
import Latex2Lean.Analysis.Monad
import Latex2Lean.Analysis.FromCsvs

/-!
The analysis we run is a static analysis that decides certain information about
nodes and variables and things. It comes in the form of a monad that can be ran
over that information and use it as context.
-/
