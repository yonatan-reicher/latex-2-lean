import Latex2Lean.Node.Basic
import Std.Data.HashSet

/-!
This file defines the analysis monad, which is a reader monad with the
information about the static analysis we did to the mathematical expressions.
-/


namespace Latex2Lean


open Std (HashSet)


instance {α} [BEq α] [Hashable α] : BEq (HashSet α) where
  beq s1 s2 := s1.toList == s2.toList


structure Analysis where
  /-- A set of all nodes that are finite sets -/
  isFiniteSet : HashSet Node := {}
  /-- A set of all nodes that are used as finite sets -/
  mustBeFiniteSet : HashSet Node := {}
  deriving Repr, Inhabited, BEq


/-- A monad transformer with the analysis information -/
abbrev AnalysisReaderT (m) [Monad m] := ReaderT Analysis m
/-- A reader monad with the analysis information -/
abbrev AnalysisReaderM := AnalysisReaderT Id


