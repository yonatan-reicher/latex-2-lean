import Latex2Lean.Latex.Basic
import Lean


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


private def getInside (delim : String)
: StateM Substring (Option $ Nat × Substring) := OptionT.run do
  let text <- get
  let dl := delim.length
  let beforeLhsDelim <- text.index? delim
  let extractStart := beforeLhsDelim + dl
  let rest := text.drop extractStart
  let toExtract <- rest.index? delim
  set $ rest.drop $ toExtract + dl
  return (beforeLhsDelim, rest.take toExtract)


-- #eval getInside "$" "text $ arstasrt $ world"


namespace Latex


-- TODO: Rename?
partial def getMathModeText
(text : Substring)
(acc : Array Substring := #[])
: Array MathModeText :=
  match step text with
  | some (inside, rest) => getMathModeText rest (acc := acc.push inside)
  | none => acc
where
  step (text : Substring) : Option $ Substring × Substring :=
    match getInside "$" text, getInside "$$" text with
    | (some (lPos, lInside), lRest), (some (rPos, rInside), rRest) =>
      some $ if lPos < rPos then (lInside, lRest) else (rInside, rRest)
    | (some (_, inside), rest), _
    | _, (some (_, inside), rest) => some (inside, rest)
    | _, _ => none


/-
/-
We need a parser `notMath` that basically parses any kind of token. Tried:
def notMath := Lean.Parser.notSymbol "$"
This definition does not work. As it turns out, Lean.Parser.notSymbol does not
consume anything, it just checks that the next tokens is not a symbol. We need a
parser that actually consumes anything that isn't the start of math mode.
For now, the following definition will do:
-/
syntax notMath := ident

declare_syntax_cat latexTextAtom
syntax " $ " latexMath " $ " : latexTextAtom
-- TODO: There is a way to make this pretty-print with newlines
syntax " $$ " latexMath " $$ " : latexTextAtom
syntax notMath : latexTextAtom

syntax latexTextAtom* : latexText
-/
