import Latex2Lean.Text
import Latex2Lean.InlineMath
import Latex2Lean.Token


namespace Latex2Lean


private def lexSingle : Text.M Token :=
  sorry


open Text.T ( skipWhitespace eof advance ) in
private def lex' : Text.M (Array Token) := do
  let mut spans := #[]
  repeat
    skipWhitespace
    if ←eof then break
    spans := spans.push (←lexSingle)
  return spans


attribute [local semireducible] Text.T
def lex : Subarray Char → (start : Pos) → Array Token
  | text, start => lex'.run text start 0 |> fun (tokens, _, _) => tokens
