module

public import Latex2Lean.Node.Basic

public section



namespace Latex2Lean.Node
 

private def listToString : List String -> String
  | [] => "nil"
  | head :: tail => s!"[ {head}, {listToString tail} ]"


def toString : Node -> String
  | { name, children } =>
    let childStrings := children.map toString
    s!"[ {name}, {listToString childStrings} ]"


instance : ToString Node where
  toString := Node.toString