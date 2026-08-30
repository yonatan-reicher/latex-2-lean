module

public import Latex2Lean.Analysis.Basic
public import Latex2Lean.Analysis.Monad
public import Latex2Lean.Analysis.FromCsvs

public section


/-!
The analysis we run is a static analysis that decides certain information about
nodes and variables and things. It comes in the form of a monad that can be ran
over that information and use it as context.
-/