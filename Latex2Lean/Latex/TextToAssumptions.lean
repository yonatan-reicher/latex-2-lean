import Latex2Lean.Latex.TextToNodes


namespace Latex


partial def textToAssumptions
(x : Substring)
: Except Error (Array Assumption × Array Node) := do
  -- Turn the text to nodes, then nodes to assumptions.
  let mut as : Array Assumption := #[]
  let mut ns : Array Node := #[]
  for (n : Node) in (<- textToNodes x) do
    let a := Assumption.mk n
    let isValid := a.toAssumptionKind.isOk
    if isValid
    then as := as.push a
    else ns := ns.push n
  return (as, ns)
