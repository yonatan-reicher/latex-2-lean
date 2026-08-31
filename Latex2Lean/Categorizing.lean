module

public import Latex2Lean.Util
public import Latex2Lean.Formula
public import Latex2Lean.CategorizedFormula
public meta import Latex2Lean.Formula

public section



namespace Latex2Lean

def categorize (id : Formula.Id) : Formula → CategorizedFormula
  | .mk opId <| .binOp (.mk varId <| .var name nameRange) .eq right =>
    .definition id name nameRange right varId opId
  | f@(.mk _ <| .binOp _ op _) => if op.predicative then .axiom_ id f else .plain id f
  | f@(.mk _ <| .forall_ ..) => .axiom_ id f
  | f@(.mk _ <| .exists_ ..) => .axiom_ id f
  | f => .plain id f


#guard
  categorize 0 (.mk 1 <| .binOp (.mk 2 <| .var "X" default) .subseteq (.mk 3 <| .var "Y" default))
  |> fun | .axiom_ .. => true | _ => false
