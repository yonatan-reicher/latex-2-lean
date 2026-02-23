import Latex2Lean.Util
import Latex2Lean.CategorizedFormula
import Latex2Lean.Analysis
import Latex2Lean.Souffle
import Latex2Lean.Node


/-!
Defines the analysis stage, which takes the categorized formulas and runs the
static analysis defined by the Souffle code on them. In order to send the
assumptions to the static analysis, we actually need to send them as strings in
CSV files, in a format we refer to as "nodes".
-/


namespace Latex2Lean


private def BinOp.toNodeName : BinOp → String
  | .plus => "+"
  | .minus => "-"
  | .star => "*"
  | .slash => "/"
  | .eq => "="
  | .cap => "intersect"
  | .cup => "union"
  | .in_ => "in"


mutual

partial def Formula.toNode : Formula → Node
  | .emptySet _ => ⟨"new-set", []⟩
  | .var name _ => ⟨String.mk name.toList, []⟩
  | .number n _ => ⟨toString n, []⟩
  | .abs inner _ => ⟨"abs", [inner.toNode]⟩
  | .binOp left op right => ⟨op.toNodeName, [left.toNode, right.toNode]⟩
  | .simpleSet elements _ => ⟨"new-set", elements.toList.map toNode⟩
  | .mapSet lhs binders _ => ⟨"map", lhs.toNode :: binders.toList.map Formula.Binder.toNode⟩
  | .tuple elements _ => ⟨"tuple", elements.toList.map toNode⟩

partial def Formula.Binder.toNode : Formula.Binder → Node
  | .in_ name set => ⟨"in", [⟨String.mk name.toList, []⟩, set.toNode]⟩

end


private def csvs (formulas : Subarray CategorizedFormula) : Array Csv := Id.run do
  let mut assumptions := #[]
  let mut expressions := #[]
  for f in formulas do
    let isAssumption := match f with
      | .definition .. | .axiom_ .. => true
      | .plain .. => false
    let node := f.toFormula.toNode
    let string := if isAssumption then s!"[ {node.toString} ]" else node.toString
    let row := #[string].toVector
    if isAssumption
    then assumptions := assumptions.push row
    else expressions := expressions.push row
  return #[
    Csv.mk (n:=1) "assumption.csv" assumptions,
    Csv.mk (n:=1) "expr.csv" expressions,
  ]


def analyze (formulas : Subarray CategorizedFormula) : IO Analysis := do
  let csvs := csvs formulas
  let result <- Souffle.call csvs (wsl := false)
  AnalysisResult.fromCsvs result.toList |> IO.ofExcept


/-- info: true -/
#guard_msgs in #eval do
  let a ← analyze #[].toSubarray
  return a == default

/-- info: true -/
#guard_msgs in #eval do
  let a ← analyze #[
      CategorizedFormula.definition "A" default (Formula.var "A" default),
    ].toSubarray
  return a == default

/-- info: true -/
#guard_msgs in #eval do
  let a ← analyze #[
      CategorizedFormula.definition "A" default (Formula.emptySet default),
    ].toSubarray
  return a == {
    isFiniteSet := .ofArray #[ ⟨"A", []⟩, ⟨"new-set", []⟩ ],
    mustBeFiniteSet := .ofArray #[],
    : Analysis
  }
