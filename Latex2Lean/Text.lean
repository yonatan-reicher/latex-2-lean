import Latex2Lean.Util
import Latex2Lean.Pos


namespace Latex2Lean.Text


abbrev Text := Subarray Char
abbrev Index := Nat


instance : Coe String Text where
  coe s := s.toList.toArray.toSubarray


@[irreducible]
def T m [Monad m] := ReaderT Text <| StateT Pos <| StateT Index <| m
abbrev M := T Id


attribute [local simp, local semireducible] T


variable {m} [Monad m]


instance {α} [Inhabited α] : Inhabited (T m α) := by simp; infer_instance
instance : Monad (T m) := by simp; infer_instance
instance : MonadReader Text (T m) := by simp; infer_instance
local instance : MonadStateOf Pos (T m) := by simp; infer_instance
local instance : MonadStateOf Index (T m) := by simp; infer_instance
instance [Alternative m] : Alternative (T m) := by simp; infer_instance
instance {ε} [MonadExceptOf ε m] : MonadExceptOf ε (T m) := by simp; infer_instance
instance {ε} [MonadExcept ε m] : MonadExcept ε (T m) where
  throw e _text _pos _idx := throw e
  tryCatch x handler text pos idx :=
    tryCatch (x.run text pos idx) fun e => (handler e).run text pos idx
instance : MonadLift m (T m) where
  monadLift x _text pos idx := show m _ from return ((←x, pos), idx)
instance : MonadLift M (T m) where
  monadLift x _text pos idx := return x.run _text pos idx



def T.mapEffect {m n α} [Monad m] [Monad n]
  (f : ∀ {α}, m α → n α) (x : T m α) : T n α :=
  fun text pos idx => f (x.run text pos idx)


def T.bindEffect {m n α} [Monad m] [Monad n]
  (f : ∀ {α}, m α → n α) (x : T m α) : T n α :=
  fun text pos idx => f (x.run text pos idx)


def T.text : M Text := read
def T.pos : M Pos := getThe _
def T.idx : M Index := getThe _
def T.peek : M (Option Char) := return (←text)[←idx]?
def T.advance : T Option Unit := do
  let some c ← T.peek | failure
  modifyThe Pos (Pos.advance c)
  modifyThe Index (· + 1)
def T.rest : M Text := return (←text)[←idx:]
def T.eof : M Bool := return (←idx) ≥ (←text).size


def T.map {α β} (f : α → β) (t : T m α) := f <$> t


def T.maybe' {α} (t : T (OptionT m) α) : T m (Option α) :=
  fun text pos idx => do
    match ←t.run text pos idx with
    | some ((a, pos'), idx') => return ((some a, pos'), idx')
    | none => return ((none, pos), idx)
def T.maybe {α} : T Option α → T Id (Option α) := T.maybe' (m := Id)


def T.skipWhile (p : Char → Bool) : T m Unit := do
  repeat
    let some c ← T.peek | return
    if p c then T.advance.maybe.map ignore
    else return


def T.skipWhitespace : T m Unit :=
  T.skipWhile Char.isWhitespace


def T.run {α}
  (text : Text)
  (pos : Pos)
  (idx : Index)
  (t : T m α)
  : m (α × Pos × Index) :=
  t text pos idx
  |> Functor.map fun ((a, pos), idx) => (a, pos, idx)


def T.run' {α}
  (text : Text)
  (pos : Pos)
  (idx : Index)
  (t : T m α)
  : m α :=
  t.run text pos idx
  |> Functor.map fun (r, _, _) => r
