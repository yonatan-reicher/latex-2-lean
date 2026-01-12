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


mutual


private def atom : T Option Formula := do
  let t ← pop
  match t.kind with
  | Token.Kind.number n => return .number n t.range
  | Token.Kind.command' r"\emptySet"
  | Token.Kind.command' r"\varnothing" => return .emptySet t.range
  | Token.Kind.command c => throw (t.range, s!"Invalid command {c}")
  | Token.Kind.word name => return .var name t.range


private def expr : M Formula := do
  let a ← atom
  sorry


end


def parse : InlineMath.Kind → Subarray Token → Formula
  | kind, tokens => expr tokens
