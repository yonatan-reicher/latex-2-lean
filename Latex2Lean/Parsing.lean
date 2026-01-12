import Latex2Lean.InlineMath
import Latex2Lean.Token
import Latex2Lean.Formula


namespace Latex2Lean


def Error := Range × String
def T m := StateT (Subarray Token) (ExceptT Error m)
abbrev M := T Id

variable {m} [Monad m]
instance : Monad (T m) := by unfold T; infer_instance
instance : MonadExcept Error (T m) := by unfold T; infer_instance
instance {ε} [MonadExceptOf ε m] : MonadExceptOf ε (T m) := by unfold T; infer_instance

def T.maybe {α} (x : T Option α) : M (Option α) :=
  fun tokens =>
    match x tokens with
    | some (.ok (ret, tokens)) => .ok (some ret, tokens)
    | some (.error e) => .error e
    | none => .ok (none, tokens)

attribute [reducible] T in
def rest : M (Subarray Token) := get 
def peek : M (Option Token) := return (←rest)[0]?
def pop : T Option Token :=
  fun tokens => show Option _ from do
    let t ← tokens[0]?
    return .ok (t, tokens[1:])

def popEq (kind : Token.Kind) : M Bool := do
  let some t ← peek
    | return false
  if t.kind = kind then
    let _ ← pop.maybe
    return true
  else
    return false


mutual


private def atom : T Option Formula := do
  let t ← pop
  match t.kind with
  | Token.Kind.number n => return .number n t.range
  | Token.Kind.command' r"\emptySet"
  | Token.Kind.command' r"\varnothing" => return .emptySet t.range
  | Token.Kind.command c => throw (t.range, s!"Invalid command '{c}'")
  | Token.Kind.word name => return .var name t.range
  | Token.Kind.symbol s => throw (t.range, s!"Invalide symbol '{s}'")


private def expr : M Formula := do
  let some t ← peek
    | panic! ""
  let some a ← atom.maybe
    | throw (t.range, "Expression must start with an atom")
  -- Set variable
  if ← popEq (.symbol' "=") then
    let Formula.var n r := a
      | panic! ""
    sorry
  else
    binaryExpr a
where
  binaryExpr lhs := do
    let op ← binaryOperator.maybe
    sorry
  binaryOperator : T Option String := do
    let t ← pop
    match t.kind with
    | Token.Kind.symbol' "+" => return "+"
    | Token.Kind.symbol' "-" => return "-"
    | Token.Kind.symbol' "*" => return "*"
    | Token.Kind.symbol' "/" => return "/"
    | Token.Kind.command' "notin" => return "notin"
    | Token.Kind.command' "in" => return "in"
    | Token.Kind.command' "subset" => return "subset"
    | Token.Kind.command' "subseteq" => return "subseteq"
    | Token.Kind.command' "supset" => return "supset"
    | Token.Kind.command' "supseteq" => return "supseteq"
    | Token.Kind.command' "cap" => return "intersect"
    | Token.Kind.command' "cup" => return "union"
    | Token.Kind.command' "times" => return "cross"
    | _ => none


end


def parse : InlineMath.Kind → Subarray Token → Except Error Formula
  | _kind, tokens =>
    expr tokens
    |> Except.map (·.1)
