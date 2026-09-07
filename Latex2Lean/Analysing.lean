module

public import Latex2Lean.Util
public import Latex2Lean.CategorizedFormula
public import Latex2Lean.Analysis
public import Latex2Lean.RunAnalysisProcess
public meta import Latex2Lean.Analysis
public meta import Latex2Lean.RunAnalysisProcess
public meta import Latex2Lean.CategorizedFormula

public section



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
  | .land => "land"
  | .lor => "lor"
  | .lt => "lt"
  | .le => "le"
  | .gt => "gt"
  | .ge => "ge"


abbrev commaSep {α} [ToString α] (l : List α) : String := ",".intercalate <| l.map toString
abbrev commaSepIds (l : List Formula) := commaSep <| l.map Formula.id


instance : ToString (Array Char) where toString := String.ofList ∘ Array.toList


def Formula.toAnalysisInputLine (f : Formula) : String :=
  s!"{show Nat from f.id},{dispatch f.kind}"
where
  dispatch : Kind → String
  | .emptySet _ _ => s!"set"
  | .var name _ => s!"var,{name}"
  | .number n _ => s!"num,{n}"
  | .app f x => s!"app,{f.name},{commaSepIds x.toList}"
  | .binOp left .eq right => s!"eq,{left.id},{right.id}"
  | .binOp left op right => s!"op,{op},{left.id},{right.id}"
  | .simpleSet .set elements _ => s!"set,{commaSepIds elements.toList}"
  | .simpleSet .multiset elements _ => s!"multiset,{commaSepIds elements.toList}"
  | .set k lhs rhs _ =>
    let kind := match k with
      | .set => "set"
      | .multiset => "multiset"
    s!"{kind},{lhs.id},{commaSepIds rhs.toList}"
  | .tuple elements _ => s!"tuple,{commaSepIds elements.toList}"
  | .quantified q binders rhs _ => s!"{q.name},{rhs.id},{commaSepIds <| binders.toList}"

partial def Formula.Binder.toAnalysisInputLine (b : Binder) : String :=
  b.toFormula.toAnalysisInputLine

def CategorizedFormula.toAnalysisInputLine : CategorizedFormula → String
  | .definition id (opId:=rootId) .. => s!"{id},definition,{rootId}"
  | .axiom_ id f => s!"{id},axiom,{f.id}"
  | .plain id f => s!"{id},plain,{f.id}"

partial def makeAnalysisInput (roots : Array CategorizedFormula) : String :=
  "\n".intercalate <|
    "id,kind,arguments"
    :: (roots.map (·.toAnalysisInputLine) |>.toList)
    ++ (roots.flatMap (toAnalysisInputLineRecursive ·.toFormula) |>.toList)
where
  toAnalysisInputLineRecursive (f : Formula) : Array String :=
    let (childFormulas, childBinders) := f.children
    #[ f.toAnalysisInputLine ]
    ++ childFormulas.flatMap toAnalysisInputLineRecursive
    ++ childBinders.flatMap (toAnalysisInputLineRecursive ·.toFormula)

def analyze (formulas : Subarray CategorizedFormula) : IO Analysis := do
  let input := makeAnalysisInput formulas
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
      CategorizedFormula.definition 0 "A" default (.mk 1 <| .var "A" default) 2 3,
    ].toSubarray
  return a == default

/-- info: true -/
#guard_msgs in
#eval do
  let a ← analyze #[
      CategorizedFormula.definition 0 "A" default (.mk 1 <| .simpleSet .set #[.mk 4 <| .number 1 default] default) 2 3,
      .plain 5 <| .mk 6 <| .var "A" default,
    ].toSubarray
  return a == {
    isFiniteSet := .ofArray #[ 1, 2, 6 ],
    mustBeFiniteSet := .ofArray #[],
    : Analysis
  }
