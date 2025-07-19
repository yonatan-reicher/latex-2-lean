import Latex2Lean.Node.Basic
 

namespace Node


private def listToString : List String -> String
  | [] => "nil"
  | head :: tail => s!"[ {head}, {listToString tail} ]"


def toString : Node -> String
  | { name, children } =>
    let childStrings := children.map toString
    s!"[ {name}, {listToString childStrings} ]"


end Node


namespace Assumption


def toString (a: Assumption) : String :=
  s!"[ {a.expr.toString} ]"


end Assumption

