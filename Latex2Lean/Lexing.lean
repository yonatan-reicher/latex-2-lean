import Latex2Lean.Text
import Latex2Lean.InlineMath
import Latex2Lean.Token


namespace Latex2Lean


private def charIf (pred : Char → Bool) : Text.T Option Char := do
  let some c ← Text.T.peek | failure
  if pred c then
    Text.T.advance
    return c
  else failure


private def manyChars (pred : Char → Bool) : Text.M (Array Char) := do
  let mut chars := #[]
  repeat
    -- Get a char
    let some c ← Text.T.peek | break
    -- Make sure it's accepted
    if not (pred c) then break
    -- Add it
    chars := chars.push c
    let _ ← Text.T.advance.maybe
  return chars

#guard
  manyChars Char.isAlpha
  |>.run "hello world" Pos.initial 0
  |> Id.run
  |> (· == ("hello".toList.toArray, ⟨1, 6⟩, 5))


private def word : Text.M Token.Kind := .word <$> manyChars Char.isAlpha


private def number : Text.M Token.Kind := do
  let digits ← manyChars Char.isDigit
  let numStr := String.mk digits.toList
  -- We know this call will succeed because we only collected digits
  let num := numStr.toNat!
  return .number num


private def symbols : Array String := #[
  -- Short
  "+","-","*","/","=","<",">","(",")","[","]","{","}",
  ",",".",";",":","!","?","&","%","$","#","^","_","~",
  "`","|","\\",
  -- Long
  "..",
]
#guard symbols.map (·.length) |>.foldl max 0 |> (· == 2)
private def symbolStarts : Array Char := symbols.filterMap fun s => s.get 0
private def isSymbolSecondChar (first : Char) (second : Char) : Bool :=
  symbols.contains (String.mk [first, second])

private def error : Text.M Token.Kind := .error <$> manyChars (!·.isWhitespace)


private def symbol : Text.T Option Token.Kind := do
  let some c1 ← Text.T.peek | failure
  if not (symbolStarts.contains c1) then failure
  Text.T.advance
  match ← Text.T.peek with
  | none => return .symbol #[c1]
  | some c2 =>
    if isSymbolSecondChar c1 c2 then
      let _ ← Text.T.advance
      return .symbol #[c1, c2]
    else return .symbol #[c1]


private def lexSingle : Text.M Token := do
  -- This function assumes we've already checked for Eof.
  let startPos ← Text.T.pos
  let some c ← Text.T.peek | panic! "lexSingle: unexpected eof"
  -- The main part of the function
  let kind ←
    -- word
    if c.isAlpha then word
    -- command
    else if c = '\\' then do
      let _ ← Text.T.advance.maybe
      let nameChars ← manyChars Char.isAlpha
      pure (.command nameChars)
    -- symbol
    else if let some s ← symbol.maybe then pure s
    -- digit
    else if c.isDigit then number
    -- error
    else error
  let endPos ← Text.T.pos
  return { kind, range := ⟨startPos, endPos⟩ }


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

-- Word
#guard
  lex "hello world" Pos.initial
  |> Array.map (·.kind)
  |> (· == #[.word' "hello", .word' "world"])
-- Number
#guard
  lex "hello 1234" Pos.initial
  |> Array.map (·.kind)
  |> (· == #[.word' "hello", .number 1234])
-- Command
#guard
  lex r"\all your base" Pos.initial
  |> Array.map (·.kind)
  |> (· == #[.command' "all", .word' "your", .word' "base"])
-- Symbol
#guard
  lex "x + y" Pos.initial
  |> Array.map (·.kind)
  |> (· == #[.word' "x", .symbol' "+", .word' "y"])
#guard
  lex "(x, y)," Pos.initial
  |> Array.map (·.kind)
  |> (· == #[.symbol' "(", .word' "x", .symbol' ",", .word' "y", .symbol' ")", .symbol' ","])
#guard
  lex "0..1" Pos.initial
  |> Array.map (·.kind)
  |> (· == #[.number 0, .symbol' "..", .number 1])
-- Ranges
#guard
  lex "xy 123" Pos.initial
  |> Array.map (·.range)
  |> (· == #[⟨⟨1, 1⟩, ⟨1, 3⟩⟩, ⟨⟨1, 4⟩, ⟨1, 7⟩⟩])
