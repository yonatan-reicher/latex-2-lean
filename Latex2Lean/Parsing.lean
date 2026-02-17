import Latex2Lean.Util
import Latex2Lean.InlineMath
import Latex2Lean.Token
import Latex2Lean.Formula


namespace Latex2Lean


def Error := Range × String
deriving instance DecidableEq, Repr, BEq for Error

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


private def setFromRange (a b : Nat) (r : Range) : Formula :=
  let len := b - a
  Array.range len
  |>.map (· + a)
  |>.map (.number · r)
  |> (.simpleSet · r)


private partial def commaSeparated
(name : String) (inThing : String)
{α} (p : T Option α)
: M (Array α) := do
  let start ← range
  let some first ← p.maybe
    | return #[]
  if (← popEq (Token.Kind.symbol' ",") |>.maybe).isSome then
    let some rest ← (commaSeparated name inThing p).maybe
      | throw (start ∪ (←range), s!"Expected {name} after ',' in {inThing}")
    return #[first] ++ rest
  else
    return #[first]


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
  | Token.Kind.command' "emptySet"
  | Token.Kind.command' "varnothing" => return .emptySet t.range
  | Token.Kind.number n =>
    (do
      let t2 ← peek
      popEq (Token.Kind.symbol' "..")
      match ←pop.maybe with
      | some { kind := Token.Kind.number m, range := r } =>
        return setFromRange n m (t.range ∪ r)
      | _ =>
        throw (t.range ∪ t2.range, "Expected a number after '..'"))
    <|> (do return .number n t.range)
  | Token.Kind.command' "abs" =>
    let some inner ← atom.maybe
      | throw (t.range, r"Expected an expression atom after '\abs'")
    return .abs inner (t.range ∪ inner.range)
  | Token.Kind.symbol' "{" =>
    let some inner ← expr.maybe
      | throw (t.range, "Expected an expression inside '{ }' (Maybe you meant to
        use '\\set{ }' for sets?)")
    popEq (Token.Kind.symbol' "}")
    <|> throw (t.range ∪ inner.range, "A '{' was not closed with an '}'")
    return inner
  | Token.Kind.command' "{" =>
    let inner ← setInsides
    let r := t.range ∪ (←range)
    let inner := inner r
    popEq (Token.Kind.command' "}")
    <|> throw (r, r"A '\{' was not closed with an '\}'")
    return inner
  | Token.Kind.command' "set" =>
    popEq (Token.Kind.symbol' "{")
    <|> throw (t.range, r"Expected '{' after '\set'")
    let inner ← setInsides
    let r := t.range ∪ (←range)
    popEq (Token.Kind.symbol' "}")
    <|> throw (r, r"A '\set{' was not closed with a '}'")
    return inner r
  | Token.Kind.symbol' r"(" =>
    let inner ← commaSeparated "an expression" "tuple" expr
    let r := t.range ∪ (← range)
    popEq (Token.Kind.symbol' ")")
    <|> throw (r, r"A '(' was not closed with a ')'")
    match h : inner.size with
    | 0 => throw (r, r"Tuples cannot have zero elements")
    | 1 => return inner[0]
    | _ => return .tuple inner r
  | Token.Kind.word name => return .var name t.range
  -- TODO: Maybe we want to just return none?
  | Token.Kind.command c => throw (t.range, s!"Invalid command '{c}'")
  | Token.Kind.symbol s => throw (t.range, s!"Invalide symbol '{s}'")
  | Token.Kind.error s => throw (t.range, s!"Lexing error: {s}")


private partial def setInsides : M (Range → Formula) := do
  let some lhs ← expr.maybe
    | return .emptySet
  if (← popEq (Token.Kind.command' "mid") |>.maybe).isSome then
    let rhs ← binders
    if rhs.isEmpty then
      throw (← range, r"Expected at least one binder after '\mid' in a set")
    return .mapSet lhs rhs
  else
    if (← popEq (Token.Kind.symbol' ",") |>.maybe).isNone then
      return Formula.simpleSet #[lhs]
    let rest ← expressions
    let all := #[lhs] ++ rest
    return .simpleSet all
where
  expressions := commaSeparated "an expression" "set" expr
  binders := commaSeparated "a binder" "set" binder


private partial def binder : T Option Formula.Binder := do
  let t ← pop
  let Token.Kind.word name := t.kind
    | failure
  popEq (Token.Kind.command' "in")
    <|> throw (t.range, r"Expected '\in' after binder variable name")
  let some rhs ← expr.maybe
    | throw (t.range ∪ (←range), r"Expected an expression after '\in' in a binder")
  return .in_ name rhs

-- setInsides ::=
--   | // empty set
--   | expr "\mid" ( expr "," )* expr // map
--   | ( expr "," )* expr // simple set


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
