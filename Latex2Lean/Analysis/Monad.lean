import Latex2Lean.Analysis.Basic
import Std


/-!
This file is all about this one Monad that we want to use to read information
about nodes. Information like, should this node be represented as a `Finset` or
as a `Set`.
-/


open Std (HashSet)

 
namespace AnalysisReaderT


variable {α}
variable {m} [Monad m]


def isFiniteSet (node : Node) : AnalysisReaderT m Bool := do
  let result <- read
  return result.isFiniteSet.contains node


def mustBeFiniteSet (node : Node) : AnalysisReaderT m Bool := do
  let result <- read
  return result.mustBeFiniteSet.contains node


def run (analysis : Analysis) (action : AnalysisReaderT m α) : m α :=
  ReaderT.run action analysis


instance {m n} [Monad m] [Monad n] [MonadLift m n]
: MonadLift (AnalysisReaderT m) (AnalysisReaderT n) where
  monadLift action analysisResult := liftM $ action.run analysisResult


end AnalysisReaderT


-- I don't want to prefix these names inside `do` notation
export AnalysisReaderT (isFiniteSet mustBeFiniteSet)
