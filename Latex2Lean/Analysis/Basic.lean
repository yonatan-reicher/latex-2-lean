import Std
import Latex2Lean.Node.Basic


open Std (HashSet)

 
structure Analysis where
  isFiniteSet : HashSet Node := {}
  mustBeFiniteSet : HashSet Node := {}
  deriving Repr, Inhabited


abbrev AnalysisReaderT (m) [Monad m] := ReaderT Analysis m
abbrev AnalysisReaderM := AnalysisReaderT Id


