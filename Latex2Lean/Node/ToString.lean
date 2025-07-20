import Latex2Lean.Node.Basic
 

private def listToString : List String -> String
  | [] => "nil"
  | head :: tail => s!"[ {head}, {listToString tail} ]"


def Node.toString : Node -> String
  | { name, children } =>
    let childStrings := children.map toString
    s!"[ {name}, {listToString childStrings} ]"


instance : ToString Node where
  toString := Node.toString
