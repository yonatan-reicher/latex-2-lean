import Latex2Lean.Latex.Basic
import Lean


-- Let us use strings as substrings of themselves
private instance : Coe String Substring where
  coe s := s.toSubstring


private instance {m} [Monad m] : MonadLift Option (OptionT m) where
  monadLift a := OptionT.mk $ pure a


private partial def Substring.index? (text : Substring) (p : String)
(start := 0)
: Option Nat := do
  let rest := text.drop start
  if rest.dropPrefix? p |>.isSome then return start
  if rest.isEmpty then failure
  text.index? p (start := start + 1)


private def getInside (delim : String) (text : Substring)
: Option $ Nat × Nat × Nat × Nat := do
  let dl := delim.length
  let outerLeft <- text.index? delim
  let innerLeft := outerLeft + dl
  let rest := text.drop innerLeft
  let innerRight <- rest.index? delim |>.map (. + innerLeft)
  let outerRight := innerRight + dl
  return (outerLeft, innerLeft, innerRight, outerRight)


#guard getInside "$" "text $ arstasrt $ world" = .some (5, 6, 16, 17)


namespace Latex


/-- Extract the math mode regions from some string of text. -/
partial def getMathModeText (text: Substring) : Array (MathModeText × Nat)
:= Id.run do
  let mut pos := 0
  let mut text := text
  let mut ret := #[]
  while true do
    match step text with
    | some (_outerLeft, innerLeft, innerRight, outerRight) =>
      let inside := text.drop innerLeft |>.take (innerRight - innerLeft)
      -- The positions given need to be adjusted
      ret := ret.push (inside, innerLeft + pos)
      text := text.drop outerRight
      pos := pos + outerRight
    | none => break
  return ret
where
  step (text : Substring) :=
    match getInside "$" text, getInside "$$" text with
    | some a, some b => some $ if a.1 < b.1 then a else b
    | some a, _ | _, some a => some a
    | none, none => none
