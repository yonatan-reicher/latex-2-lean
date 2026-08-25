import Latex2Lean.Util
import Latex2Lean.CategorizedFormula
import Latex2Lean.Analysis
import Latex2Lean.RunAnalysisProcess


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
  | .subseteq => "subseteq"
  | .subset => "subset"
  | .supseteq => "supseteq"
  | .supset => "supset"
  | .times => "times"


abbrev commaSep {α} [ToString α] (l : List α) : String := ",".intercalate <| l.map toString
abbrev commaSepIds (l : List Formula) := commaSep <| l.map Formula.id


instance : ToString (Array Char) where toString := Coe.coe


def Formula.toAnalysisInputLine (f : Formula) : String :=
  s!"{show Nat from f.id},{dispatch f.kind}"
where
  dispatch : Kind → String
  | .emptySet _ _ => s!"set"
  | .var name _ => s!"var,{name}"
  | .number n _ => s!"num,{n}"
  | .app f x => s!"app,{f.name},{x.id}"
  | .binOp left .eq right => s!"eq,{left.id},{right.id}"
  | .binOp left op right => s!"op,{op},{left.id},{right.id}"
  | .simpleSet .set elements _ => s!"set,{commaSepIds elements.toList}"
  | .simpleSet .multiset elements _ => s!"multiset,{commaSepIds elements.toList}"
  | .mapSet _ lhs binders _ => s!"map,{lhs.id},{commaSepIds <| binders.toList.map (·.toFormula)}"
  | .tuple elements _ => s!"tuple,{commaSepIds elements.toList}"
  | .forall_ binders rhs _ => s!"forall,{rhs.id},{commaSepIds <| binders.toList.map (·.toFormula)}"

partial def Formula.Binder.toAnalysisInputLine (b : Binder) : String :=
  b.toFormula.toAnalysisInputLine

partial def makeAnalysisInput (roots : Array Formula) : String :=
  "\n".intercalate <|
    "id,kind,arguments"
    :: (roots.flatMap toAnalysisInputLineRecursive |>.toList)
where
  toAnalysisInputLineRecursive (f : Formula) : Array String :=
    let (childFormulas, childBinders) := f.children
    #[ f.toAnalysisInputLine ]
    ++ childFormulas.flatMap toAnalysisInputLineRecursive
    ++ childBinders.flatMap (toAnalysisInputLineRecursive ·.toFormula)

def analyze (formulas : Subarray CategorizedFormula) : IO Analysis := do
  let input := makeAnalysisInput <| formulas.toArray.map (·.toFormula)
  let (_stdout, outputs) ← runAnalysisProcess input
  -- IO.println stdout
  .ofExcept <| Analysis.fromCsvs <| outputs.toList.map fun (name, csv) =>
    -- rename
    { csv with fileName := name }


/-- info: true -/
#guard_msgs in #eval do
  let a ← analyze #[].toSubarray
  return a == default

/-- info: true -/
#guard_msgs in
#eval do
  let a ← analyze #[
      CategorizedFormula.definition "A" default (.mk 1 <| .var "A" default) 2 3,
    ].toSubarray
  return a == default

-- /-- info: true -/
-- #guard_msgs in
#eval do
  let a ← analyze #[
      CategorizedFormula.definition "A" default (.mk 1 <| .simpleSet .set #[.mk 4 <| .number 1 default] default) 2 3,
    ].toSubarray
  return a == {
    isFiniteSet := .ofArray #[ 3, 1 ],
    mustBeFiniteSet := .ofArray #[],
    : Analysis
  }
