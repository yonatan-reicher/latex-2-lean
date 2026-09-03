module

public import Latex2Lean.Util
public import Latex2Lean.Pos
public import Latex2Lean.Range

public section



namespace Latex2Lean


inductive BinOp where
  -- numeric symbols
  | plus
  | minus
  | star
  | slash
  -- other symbols
  | eq
  -- words
  | cap
  | cup
  | in_
  | times
  | subseteq
  | subset
  | supseteq
  | supset
  deriving DecidableEq, Inhabited, Repr


def BinOp.toString : BinOp → String
  | plus => "+"
  | minus => "-"
  | star => "*"
  | slash => "/"
  | eq => "="
  | cap => r"\cap"
  | cup => r"\cup"
  | in_ => r"\in"
  | times => r"\times"
  | subseteq => r"\subseteq"
  | subset => r"\subset"
  | supseteq => r"\supseteq"
  | supset => r"\supset"


/-- An operator that returns a true/false value -/
def BinOp.predicative : BinOp → Bool
  | .eq
  | .in_
  | .subseteq
  | .subset
  | .supseteq
  | .supset
    => true
  | .plus
  | .minus
  | .star
  | .slash
  | .cap
  | .cup
  | .times
    => false

def BinOp.all : Array BinOp :=
  #[
    .eq,
    .in_,
    .subseteq,
    .subset,
    .supseteq,
    .supset,
    .plus,
    .minus,
    .star,
    .slash,
    .cap,
    .cup,
    .times,
  ]

theorem BinOp.mem_all : ∀ op, op ∈ all := by
  intro op
  simp only [all, List.mem_toArray, List.mem_cons, List.not_mem_nil, or_false]
  cases op <;> decide

instance : ToString BinOp where
  toString := BinOp.toString


structure Formula.Ident where
  name : Array Char
  range : Range
  deriving Inhabited, DecidableEq, Repr


inductive SetKind
  | set
  | multiset
  deriving Inhabited, DecidableEq, Repr


@[expose]
def Formula.Id := Nat
deriving instance DecidableEq, Inhabited, Repr, ToString, Hashable for Formula.Id
instance {n} : OfNat Formula.Id n := ⟨n⟩

inductive Formula.Quantifier
  | forall_
  | exists_
  deriving DecidableEq, Hashable, Inhabited, Repr

mutual

inductive Formula.Kind where
  -- TODO: Replace names with Ident.
  | emptySet (kind : SetKind) (range : Range) -- TODO: Remove
  -- TODO: Add a data type for a name which is a string and a range.
  | var (name : Array Char) (range : Range)
  | number (n : Nat) (range : Range)
  /-- func - name of the function, may have a '\' at the start if it's some
    command like \abs or \sum. -/
  | app (func : Formula.Ident) (args : Array Formula)
  | binOp (left : Formula) (op : BinOp) (right : Formula)
  | simpleSet (kind : SetKind) (elements : Array Formula) (range : Range)
  | set (kind : SetKind) (lhs : Formula) (rhs : Array Formula) (range : Range)
  | tuple (elements : Array Formula) (range : Range)
  | quantified (q : Formula.Quantifier) (binders : Array Formula.Binder) (rhs : Formula) (range : Range)
  deriving Inhabited, BEq, Repr

/--
A formula object is an Abstact Syntax Tree of the code inside an inline-math
section of our text.
-/
structure Formula where
  id : Formula.Id
  kind : Formula.Kind
  deriving Inhabited, BEq, Repr


inductive Formula.Binder where
  | in_ (varId rootId : Formula.Id) (name : Array Char) (nameRange : Range) (set : Formula)
  deriving Inhabited, BEq, Repr

end

mutual

partial def Formula.WF (f : Formula) := f.kind.WF

partial def Formula.Kind.WF : Kind → Bool
  | .emptySet _ _
  | .var _ _
  | .number _ _
     => true
  | .app _func args => 0 < args.size ∧ args.all WF
  | .binOp l _ r => l.WF ∧ r.WF
  | .simpleSet _ elements _ => elements.all WF
  | .set _ lhs rhs _ => lhs.WF ∧ rhs.all WF ∧ ¬rhs.isEmpty
  | .tuple elements _ => elements.size > 1 ∧ elements.all WF
  | .quantified _ binders rhs _ => binders.size > 1 ∧ binders.all Binder.WF ∧ rhs.WF

partial def Formula.Binder.WF : Formula.Binder → Bool
  | .in_ (set:=inner) .. => inner.WF

end

@[expose, match_pattern] def Formula.Kind.forall_ := quantified .forall_
@[expose, match_pattern] def Formula.Kind.exists_ := quantified .exists_

def Formula.Quantifier.name
  | forall_ => "forall"
  | exists_ => "exists"

mutual

partial def Formula.Kind.range : Kind → Range
  | .emptySet _ r => r
  | .var _ r => r
  | .number _ r => r
  | .app func args => func.range ∪ args[args.size - 1]!.range
  | .binOp l _ r => l.range ∪ r.range
  | .simpleSet _ _ r => r
  | .set (range:=r) .. => r
  | .tuple _ r => r
  | .quantified _ _ _ r => r

partial def Formula.range (f : Formula) := f.kind.range

end

mutual

partial def Formula.toString (f : Formula) := f.kind.toString

partial def Formula.Kind.toString : Kind → String
  | .emptySet _ _ => "\\emptyset"
  | .var name _ => s!"{show String from name}"
  | .number n _ => s!"{n}"
  | .app func args => s!"({show String from func.name} {" ".intercalate <| args.toList.map (·.toString)})"
  | .binOp left op right =>
    s!"{left.toString} {op} {right.toString}"
  | .simpleSet _ elements _ =>
    ", ".intercalate (elements.toList.map Formula.toString)
    |> (s!"\\\{ {·} \\}")
  | .set _ lhs rhs _ =>
    s!"\\\{ {lhs.toString} \\mid {", ".intercalate (rhs.toList.map toString)} \\}"
  | .tuple elements _ =>
    s!"({", ".intercalate (elements.toList.map Formula.toString)})"
  | .quantified q #[binder] rhs _ =>
    s!"\\{q.name} {binder.toString}, {rhs.toString}"
  | .quantified q binders rhs _ =>
    binders.toList.map (s!"({·.toString})")
    |> " ".intercalate
    |> (s!"\\{q.name} {·}, {rhs.toString}")

partial def Formula.Binder.toString : Formula.Binder → String
  | .in_ _ _ name _ set => s!"{show String from name} \\in {set.toString}"

end

instance : ToString Formula := ⟨Formula.toString⟩
instance : ToString Formula.Binder := ⟨Formula.Binder.toString⟩

-- #guard
--   Formula.mk 1 <| Formula.Kind.mapSet .set
--     (Formula.mk 2 <| Formula.Kind.binOp (.var "x" default) .plus (.number 1 default))
--     #[.in_ "x" $ .emptySet .set default
--     , .in_ "y" $ .simpleSet .set #[.number 5 default] default]
--     default
--   |>.toString
--   |> (· == r"\{ x + 1 \mid x \in \emptyset, y \in \{ 5 \} \}")

mutual

def Formula.children (f : Formula) := f.kind.children

def Formula.Kind.children : Kind → Array Formula × Array Binder
  | .emptySet ..
  | .var ..
  | .number ..
    => (#[], #[])
  | .app _func args => (args, #[])
  | .binOp left _op right => (#[left, right], #[])
  | .simpleSet _ elements .. => (elements, #[])
  | .set _ lhs rhs .. => (#[lhs] ++ rhs, #[])
  | .tuple elements .. => (elements, #[])
  | .quantified _ binders rhs .. => (#[rhs], binders)

def Formula.Binder.toFormula : Binder → Formula
  | .in_ varId rootId name nameRange set =>
    .mk rootId <| .binOp (.mk varId <| .var name nameRange) .in_ set

end
