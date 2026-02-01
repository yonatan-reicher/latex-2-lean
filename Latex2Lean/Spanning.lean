import Latex2Lean.Util
import Latex2Lean.Pos
import Latex2Lean.InlineMath
import Latex2Lean.Text


/-!
This file is about taking input text and spitting out the inline math spans
inside of it.
-/


namespace Latex2Lean


open InlineMath ( Kind Span )


private def getKind : Text.T Option Kind := do
  let kind ← Kind.parseFromPrefix (←Text.T.rest)
  -- for _ in [:kind.toString.length] do Text.T.advance
  -- This doesn't work due to a compiler bug.
  let mut n := kind.toString.length
  repeat do
    Text.T.advance
    n := n - 1
  until n = 0
  return kind

#guard getKind.run "x + y" Pos.initial 0 = .none
#guard getKind.run "$x + y$" Pos.initial 0 == .some (.singleDollar, ⟨1, 2⟩, 1)


private partial def skipToNextKind : Text.T Option Kind := inner ()
where inner _ :=
  try getKind catch
  | () =>
    Text.T.advance
    inner ()


private partial def skipToNextKindEq (k : Kind) : Text.T Option Pos := do
  let p ← Text.T.pos
  try
    if (← getKind) = k
    then return p
    else failure
  catch
  | () =>
    Text.T.advance
    skipToNextKindEq k


inductive Spanning.Error
  | unmatchedDelim (kind : Kind) (pos : Pos)
  deriving DecidableEq, BEq, Repr

open Text.T in
private def span' : Text.T (Except Spanning.Error) (Array (Kind × Span)) := do
  let mut spans := #[]
  -- We must skip space the the start, to know if we have an empty file
  skipWhitespace
  while !(←eof) do
    let lhsDelimPos ← pos
    -- Get a delim
    let some k ← skipToNextKind.maybe
      | break
    skipWhitespace
    let lhsPos ← pos
    let lhsIndex ← idx
    -- Match it
    let rhsDelimPos ← skipToNextKindEq k |>.mapEffect (m := Option) fun
      | some x => .ok x
      | none => .error (Spanning.Error.unmatchedDelim k lhsDelimPos)
    let rhsIndex := (←idx) - k.toString.length
    let text := (←text)[lhsIndex:rhsIndex]
    spans := spans.push (k, Span.mk lhsDelimPos rhsDelimPos lhsPos text.toArray)
  return spans


def span (text : Subarray Char) : Except Spanning.Error (Array (Kind × Span)) :=
  span'.run text Pos.initial 0
  |>.map fun (spans, _, _) => spans


#guard span "hello" == .ok #[]
#guard
  span "hello $my sweet$ world"
  == .ok #[(.singleDollar, ⟨⟨1, 1⟩, ⟨1, 16⟩, ⟨1, 8⟩, "my sweet"⟩)]
