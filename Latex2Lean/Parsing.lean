import Latex2Lean.Util
import Latex2Lean.InlineMath
import Latex2Lean.Token
import Latex2Lean.Formula

import Lean.Message


namespace Latex2Lean


def Error := Range × String
deriving instance DecidableEq, Repr, BEq for Error

abbrev FId := Formula.Id
abbrev NextId := FId

instance : Lean.ToMessageData Error where
  toMessageData self := m!"{self.1} {self.2}"

def T m := StateT (Subarray Token) (StateT NextId (ExceptT Error m))
abbrev M := T Id

variable {m} [Monad m]
instance : Monad (T m) := unfold T in inferInstance
instance : MonadExcept Error (T m) := unfold T in inferInstance
instance [Alternative m] : Alternative (T m) := unfold T in inferInstance
instance {ε} [MonadExceptOf ε m] : MonadExceptOf ε (T m) := unfold T in inferInstance
instance : MonadStateOf NextId (T m) where
  get := fun tokens nextId => return ((nextId, tokens), nextId)
  set newNextId := fun tokens _nextId => return (((), tokens), newNextId)
  modifyGet f := fun tokens nextId =>
    let (ret, nextId) := f nextId
    return ((ret, tokens), nextId)

instance : MonadLift (T Id) (T m) where
  monadLift x :=
    fun tokens nextId =>
      match x.run tokens |>.run nextId with
      | .ok (ret, tokens) => pure (ret, tokens)
      | .error e => throw e

def T.maybe {α} (x : T Option α) : M (Option α) :=
  fun tokens nextId =>
    match x tokens nextId with
    | some (.ok ((ret, tokens), nextId)) => .ok ((some ret, tokens), nextId)
    | some (.error e) => .error e
    | none => .ok ((none, tokens), nextId)

def rest : M (Subarray Token) := unfold M T in get
def peek : T Option Token := do
  match (←rest)[0]? with
  | some t => return t
  | none => failure
def pop : T Option Token :=
  fun tokens nextId => show Option _ from do
    let t ← tokens[0]?
    return .ok ((t, tokens[1:]), nextId)

def popEq (kind : Token.Kind) : T Option Unit := do
  let t ← peek
  if t.kind = kind
  then ignore <$> pop
  else failure

def popId [MonadStateOf NextId m] : m FId :=
  modifyGetThe NextId fun nextId => (nextId, Nat.add nextId 1)

def range : M Range := do
  match ← peek.maybe with
  | none => return default
  | some t => return t.range


private def setFromRange (a b : Nat) (r : Range) : T Id Formula := do
  let len := b - a
  Array.range (len + 1)
    |>.map (· + a) -- Add the starting to all indices to get the numbers
    |>.mapM (m:=M) (fun n => return Formula.mk (← popId) <| .number n r) -- Make number nodes
    |>.map (Formula.mk (← popId) <| .simpleSet .set · r) -- Put the array in a set node


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


private partial def binaryOperator : T Option BinOp := do
  let t ← pop
  BinOp.all.find? fun x => toTokenKind x = t.kind
where toTokenKind : BinOp → Token.Kind
  | op =>
    let str := op.toString
    let c := str.get 0
    if c = some '\\'
    then .command' $ str.drop 1
    else .symbol' str

#guard binaryOperator.toTokenKind .plus = .symbol "+"
#guard binaryOperator.toTokenKind .times = .command "times"


local macro "returnNewNode " t:term:min : doElem => `(doElem|return Formula.mk (← popId) $t)


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
    returnNewNode .binOp lhs op rhs


private partial def forall_ : T Option Formula := do
  let start ← range
  -- TODO: Parse multiple binders.
  let binder ← binder
  popEq (.symbol' ",") <|> throw (start ∪ (←range), "Expected ',' after binder in '\\forall'")
  let rhs ← expr
  returnNewNode .forall_ #[binder] rhs (start ∪ rhs.range)


private partial def atom : T Option Formula := do
  let t ← pop
  match t.kind with
  | Token.Kind.command' "emptyset"
  | Token.Kind.command' "varnothing" => returnNewNode .emptySet .set t.range
  | Token.Kind.number n =>
    (do
      let t2 ← peek
      popEq (Token.Kind.symbol' "..")
      match ←pop.maybe with
      | some { kind := Token.Kind.number m, range := r } =>
        setFromRange n m (t.range ∪ r)
      | _ =>
        throw (t.range ∪ t2.range, "Expected a number after '..'"))
    <|> (do returnNewNode .number n t.range)
  | Token.Kind.command' "abs"
  | Token.Kind.command' "sum" =>
    let some inner ← atom.maybe
      | throw (t.range, r"Expected an expression atom after '\abs'")
    match t.kind with
    | Token.Kind.command' "abs" => returnNewNode .app ⟨"\\abs", t.range⟩ inner
    | Token.Kind.command' "sum" => returnNewNode .app ⟨"\\sum", t.range⟩ inner
    | _ => throw (t.range, "Invalid function name")
  | Token.Kind.symbol' "{" =>
    let some inner ← expr.maybe
      | throw (t.range, "Expected an expression inside '{ }' (Maybe you meant to
        use '\\set{ }' for sets?)")
    popEq (Token.Kind.symbol' "}")
    <|> throw (t.range ∪ inner.range, "A '{' was not closed with an '}'")
    return inner
  | Token.Kind.command' "{" =>
    let inner ← setInsides .set
    let r := t.range ∪ (←range)
    let inner := inner r
    popEq (Token.Kind.command' "}")
    <|> throw (r, r"A '\{' was not closed with an '\}'")
    returnNewNode inner
  | Token.Kind.command' "set"
  | Token.Kind.command' "mset" =>
    let kind ← match t.kind with
      | .command' "set" => pure .set
      | .command' "mset" => pure .multiset
      | _ => throw (t.range, "Invalid set kind")
    popEq (Token.Kind.symbol' "{")
    <|> throw (t.range, r"Expected '{' after '\set'")
    let inner ← setInsides kind
    let r := t.range ∪ (←range)
    popEq (Token.Kind.symbol' "}")
    <|> throw (r, r"A '\set{' was not closed with a '}'")
    returnNewNode inner r
  | .command' "forall" => forall_
  | Token.Kind.symbol' r"(" =>
    let inner ← commaSeparated "an expression" "tuple" expr
    let r := t.range ∪ (← range)
    popEq (Token.Kind.symbol' ")")
    <|> throw (r, r"A '(' was not closed with a ')'")
    match h : inner.size with
    | 0 => throw (r, r"Tuples cannot have zero elements")
    | 1 => return inner[0]
    | _ => returnNewNode .tuple inner r
  | Token.Kind.word name => returnNewNode .var name t.range
  -- TODO: Maybe we want to just return none?
  | Token.Kind.command c => throw (t.range, s!"Invalid command '{c}'")
  | Token.Kind.symbol s => throw (t.range, s!"Invalid symbol '{s}'")
  | Token.Kind.error s => throw (t.range, s!"Lexing error: {s}")


private partial def setInsides (kind : SetKind) : M (Range → Formula.Kind) := do
  let some lhs ← expr.maybe
    | return .emptySet kind
  if (← popEq (Token.Kind.command' "mid") |>.maybe).isSome then
    let rhs ← expressions
    if rhs.isEmpty then
      throw (← range, r"Expected at least one expression after '\mid' in a set")
    return .set kind lhs rhs
  else
    if (← popEq (Token.Kind.symbol' ",") |>.maybe).isNone then
      return .simpleSet kind #[lhs]
    let rest ← expressions
    let all := #[lhs] ++ rest
    return .simpleSet kind all
where
  expressions := commaSeparated "an expression" "set" expr


private partial def binder : T Option Formula.Binder := do
  let t ← pop
  let Token.Kind.word name := t.kind
    | failure
  popEq (Token.Kind.command' "in")
    <|> throw (t.range, r"Expected '\in' after binder variable name")
  let some rhs ← expr.maybe
    | throw (t.range ∪ (←range), r"Expected an expression after '\in' in a binder")
  return .in_ (← popId) (← popId) name t.range rhs

-- setInsides ::=
--   | // empty set
--   | expr "\mid" ( expr "," )* expr // map
--   | ( expr "," )* expr // simple set


end


def parse : InlineMath.Kind → Subarray Token → NextId → Except Error (Formula × NextId)
  | _kind, tokens, firstId =>
    match expr tokens firstId with
    | .error e => .error e
    | .ok ((f, rest), nextId) =>
      if h : rest.size = 0 then .ok (f, nextId)
      else
        have : NeZero rest.size := by exact { out := h }
        let first := rest.get 0
        .error (first.range,
           s!"leftover tokens - parsed program up to this point: \n{f}")


#guard
  parse
    .singleDollar
    #[ Token.mk (.word' "x") ⟨⟨0, 1⟩, ⟨2, 3⟩⟩ ]
    1
  == .ok (.mk 1 <| .var "x" ⟨⟨0, 1⟩, ⟨2, 3⟩⟩, 2)
