import Latex2Lean.Latex.TextToNodes


partial def LatexText.toAssumptions
(x : LatexText)
: Except BadLatex (List Assumption) :=
  return (<- x.toNodes).filterMap fun x =>
    let a := Assumption.mk x
    let isValid := a.toAssumptionKind.isOk
    if isValid then some a else none
