/--
This file is all about definitions relating to parsing things in strings while
respecting parentheses.
-/

-- Our parentheses are square brackets.
def lParenChar := '['
def rParenChar := ']'
def lParen := lParenChar.toString
def rParen := rParenChar.toString

/--
Return the string as a list of characters, with the parenthesis depth of the
character.
-/
def String.zipDepth (s : String) (startDepth := 0)
: Array (Char × Int) := Id.run do
  -- Must make sure `depth` is an `Int` and not a `Nat`.
  let mut depth : Int := startDepth
  let mut ret := Array.emptyWithCapacity s.length
  for c in s.toList do
    if c == lParenChar then depth := depth + 1
    else if c == rParenChar then depth := depth - 1
    ret := ret.push (c, depth)
  return ret

/--
Splits a string at the delimiter, but respects parentheses.
-/
def String.splitButParens
(text : String) (delim : Char) : Array Substring := Id.run do
  let mut depth : Int := 0
  let mut startIdx := 0
  let mut endIdx := 0
  let mut ret := #[]
  for c in text.toList do
    if c == lParenChar then depth := depth + 1
    if c == rParenChar then depth := depth - 1
    if depth == 0 && c == delim then
      let substring := Substring.mk text startIdx endIdx
      ret := ret.push substring
      startIdx := endIdx + c
      endIdx := startIdx
    else
      endIdx := endIdx + c
  let substring := Substring.mk text startIdx endIdx
  ret := ret.push substring
  return ret

-- This is a unit test for the function above
#guard
  "hello, [world, 2], 3"
  |>.splitButParens ','
  |>.map (·.toString)
  |> (. == #["hello", " [world, 2]", " 3"])


