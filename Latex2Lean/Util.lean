/-!
Missing definitions from standard types and some helpful helpers.
-/


-- Need equality for Except.
deriving instance DecidableEq, BEq for Except


namespace String

/--
Return the string as a list of characters, with the parenthesis depth of the
character.
-/
def zipDepth (s : String) (startDepth := 0) (lParen rParen : Char)
: Array (Char × Int) := Id.run do
  -- Must make sure `depth` is an `Int` and not a `Nat`.
  let mut depth : Int := startDepth
  let mut ret := Array.emptyWithCapacity s.length
  for c in s.toList do
    if c == lParen then depth := depth + 1
    else if c == rParen then depth := depth - 1
    ret := ret.push (c, depth)
  return ret


/--
Splits a string at the delimiter, but respects parentheses.
-/
def splitButParens (text : String) (delim : Char) (lParen rParen : Char)
-- TODO: : Array (Array Char) := Id.run do
: Array Substring := Id.run do
  let mut depth : Int := 0
  let mut startIdx := 0
  let mut endIdx := 0
  let mut ret := #[]
  for c in text.toList do
    if c == lParen then depth := depth + 1
    if c == rParen then depth := depth - 1
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
  |>.splitButParens ',' '[' ']'
  |>.map (·.toString)
  |> (. == #["hello", " [world, 2]", " 3"])

end String


abbrev ignore {α} (_ : α) : PUnit := ()
