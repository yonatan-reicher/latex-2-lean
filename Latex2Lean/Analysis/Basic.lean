import Latex2Lean.Node.Basic
import Std

/-!
This file defines the analysis monad, which is a reader monad with the
information about the static analysis we did to the mathematical expressions.
-/


namespace Latex2Lean


open Std (HashSet)

 
structure Analysis where
  /-- A set of all nodes that are finite sets -/
  isFiniteSet : HashSet Node := {}
  /-- A set of all nodes that are used as finite sets -/
  mustBeFiniteSet : HashSet Node := {}
  deriving Repr, Inhabited


/-- A monad transformer with the analysis information -/
abbrev AnalysisReaderT (m) [Monad m] := ReaderT Analysis m
/-- A reader monad with the analysis information -/
abbrev AnalysisReaderM := AnalysisReaderT Id


