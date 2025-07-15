import Latex2Lean.Node.Basic


namespace Node


def assert0Children (term : Node) : Except String Unit :=
  match term.children with
  | [] => .ok ()
  | _ =>
    let n := term.children.length
    .error s!"Wrong number of arguments to '{term.name}' - expected 0, got {n}"


def assert1Children (term : Node)
: Except String Node :=
  match term.children with
  | [x] => .ok x
  | _ => 
    let n := term.children.length
    .error s!"Wrong number of arguments to '{term.name}' - expected 2, got {n}"


def assert2Children (term : Node)
: Except String (Node × Node) :=
  match term.children with
  | [lhs, rhs] => .ok (lhs, rhs)
  | _ => 
    let n := term.children.length
    .error s!"Wrong number of arguments to '{term.name}' - expected 2, got {n}"


