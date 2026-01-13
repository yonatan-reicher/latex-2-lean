import Latex2Lean.Util
import Latex2Lean.InlineMath
import Latex2Lean.Token
import Latex2Lean.Formula


namespace Latex2Lean


def Error := Range × String
deriving instance DecidableEq for Error

def T m := StateT (Subarray Token) (ExceptT Error m)
abbrev M := T Id

variable {m} [Monad m]
instance : Monad (T m) := unfold T in inferInstance
instance : MonadExcept Error (T m) := unfold T in inferInstance
instance [Alternative m] : Alternative (T m) := unfold T in inferInstance
instance {ε} [MonadExceptOf ε m] : MonadExceptOf ε (T m) := unfold T in inferInstance

instance : MonadLift (T Id) (T m) where
  monadLift x :=
    fun tokens =>
      match x.run tokens with
      | .ok (ret, tokens) => pure (ret, tokens)
      | .error e => throw e

def T.maybe {α} (x : T Option α) : M (Option α) :=
  fun tokens =>
    match x tokens with
    | some (.ok (ret, tokens)) => .ok (some ret, tokens)
    | some (.error e) => .error e
    | none => .ok (none, tokens)

def rest : M (Subarray Token) := unfold M T in get
def peek : T Option Token := do
  match (←rest)[0]? with
  | some t => return t
  | none => failure
def pop : T Option Token :=
  fun tokens => show Option _ from do
    let t ← tokens[0]?
    return .ok (t, tokens[1:])

def popEq (kind : Token.Kind) : T Option Unit := do
  let t ← peek
  if t.kind = kind
  then ignore <$> pop
  else failure

def range : M Range := do
  match ← peek.maybe with
  | none => return default
  | some t => return t.range


mutual


private partial def expr : M Formula :=
  Option.get! <$> binaryExpr.maybe


private partial def binaryExpr : T Option Formula := do
  let lhs ← atom
  let opRange ← range
  match ← binaryOperator.maybe with
  | none => return lhs
  | some op =>
    let some rhs ← binaryExpr.maybe
      | throw (opRange, "Expected an expression after a binary operator")
    return .binOp lhs op rhs


private partial def binaryOperator : T Option BinOp := do
  let t ← pop
  match t.kind with
  | Token.Kind.symbol' "+" => return .plus
  | Token.Kind.symbol' "-" => return .minus
  | Token.Kind.symbol' "*" => return .star
  | Token.Kind.symbol' "/" => return .slash
  | Token.Kind.symbol' "=" => return .eq
  | Token.Kind.command' "cap" => return .cap
  | Token.Kind.command' "cup" => return .cup
  | Token.Kind.command' "in" => return .in_
  | _ => none


private partial def atom : T Option Formula := do
  let t ← pop
  match t.kind with
  | Token.Kind.number n => return .number n t.range
  | Token.Kind.command' r"\emptySet"
  | Token.Kind.command' r"\varnothing" => return .emptySet t.range
  | Token.Kind.word name => return .var name t.range
  -- TODO: Maybe we want to just return none?
  | Token.Kind.command c => throw (t.range, s!"Invalid command '{c}'")
  | Token.Kind.symbol s => throw (t.range, s!"Invalide symbol '{s}'")
  | Token.Kind.error s => throw (t.range, s!"Lexing error: {s}")


end


def parse : InlineMath.Kind → Subarray Token → Except Error Formula
  | _kind, tokens =>
    expr tokens
    |> Except.map (·.1)


#guard
  parse
    .singleDollar
    #[ Token.mk (.word' "x") ⟨⟨0, 1⟩, ⟨2, 3⟩⟩ ]
  == .ok (.var "x" ⟨⟨0, 1⟩, ⟨2, 3⟩⟩)
