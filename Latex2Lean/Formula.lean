import Latex2Lean.Util
import Latex2Lean.Pos
import Latex2Lean.Range


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


def Formula.Id := Nat
deriving instance DecidableEq, Inhabited, Repr, ToString for Formula.Id
instance {n} : OfNat Formula.Id n := ⟨n⟩

mutual

inductive Formula.Kind where
  -- TODO: Replace names with Ident.
  | emptySet (kind : SetKind) (range : Range) -- TODO: Remove
  -- TODO: Add a data type for a name which is a string and a range.
  | var (name : Array Char) (range : Range)
  | number (n : Nat) (range : Range)
  /-- func - name of the function, may have a '\' at the start if it's some
    command like \abs or \sum. -/
  | app (func : Formula.Ident) (arg : Formula)
  | binOp (left : Formula) (op : BinOp) (right : Formula)
  | simpleSet (kind : SetKind) (elements : Array Formula) (range : Range)
  | mapSet (kind : SetKind) (lhs : Formula) (binders : Array Formula.Binder) (range : Range)
  | tuple (elements : Array Formula) (range : Range)
  | forall_ (binders : Array Formula.Binder) (rhs : Formula) (range : Range)
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
  | .app _ inner => inner.WF
  | .binOp l _ r => l.WF ∧ r.WF
  | .simpleSet _ elements _ => elements.all WF
  | .mapSet _ lhs binders _ => lhs.WF ∧ binders.all Binder.WF
  | .tuple elements _ => elements.size > 1 ∧ elements.all WF
  | .forall_ binders rhs _ => binders.size > 1 ∧ binders.all Binder.WF ∧ rhs.WF

partial def Formula.Binder.WF : Formula.Binder → Bool
  | .in_ _ _ _ inner => inner.WF

end

mutual

partial def Formula.Kind.range : Kind → Range
  | .emptySet _ r => r
  | .var _ r => r
  | .number _ r => r
  | .app func arg => func.range ∪ arg.range
  | .binOp l _ r => l.range ∪ r.range
  | .simpleSet _ _ r => r
  | .mapSet _ _ _ r => r
  | .tuple _ r => r
  | .forall_ _ _ r => r

partial def Formula.range (f : Formula) := f.kind.range

end

mutual

partial def Formula.toString (f : Formula) := f.kind.toString

partial def Formula.Kind.toString : Kind → String
  | .emptySet _ _ => "\\emptyset"
  | .var name _ => s!"{show String from name}"
  | .number n _ => s!"{n}"
  | .app func arg => s!"{show String from func.name} {arg.toString}"
  | .binOp left op right =>
    s!"{left.toString} {op} {right.toString}"
  | .simpleSet _ elements _ =>
    ", ".intercalate (elements.toList.map Formula.toString)
    |> (s!"\\\{ {·} \\}")
  | .mapSet _ lhs binders _ =>
    s!"\\\{ {lhs.toString} \\mid {", ".intercalate (binders.toList.map Formula.Binder.toString)} \\}"
  | .tuple elements _ =>
    s!"({", ".intercalate (elements.toList.map Formula.toString)})"
  | .forall_ #[binder] rhs _ =>
    s!"\\forall {binder.toString}, {rhs.toString}"
  | .forall_ binders rhs _ =>
    binders.toList.map (s!"({·.toString})")
    |> " ".intercalate
    |> (s!"\\forall {·}, {rhs.toString}")

partial def Formula.Binder.toString : Formula.Binder → String
  | .in_ _ _ name set => s!"{show String from name} \\in {set.toString}"

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
    => #[]
  | .app _func arg => #[arg]
  | .binOp left _op right => #[left, right]
  | .simpleSet _ elements .. => elements
  | .mapSet _ lhs binders .. => #[lhs] ++ binders.flatMap (·.children)
  | .tuple elements .. => elements
  | .forall_ binders rhs .. => binders.flatMap (·.children) ++ #[rhs]

def Formula.Binder.children : Binder → Array Formula
  | .in_ _ _ _ _ a => #[a]

def Formula.Binder.toFormula : Binder → Formula
  | .in_ varId rootId name nameRange set =>
    .mk rootId <| .binOp (.mk varId <| .var name nameRange) .in_ set

end
