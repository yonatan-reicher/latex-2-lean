import Latex2Lean.Except
import Latex2Lean.Node.Basic
import Latex2Lean.Parentheses


/--
Parses a list of the form `[ [a, nil], [ [b, nil], [ [c, nil], nil ] ] ]`
-/
private partial def listFromString (text : String) : Except String (List String) := do
  if text.isEmpty then throw "Node.fromString: empty string"
  if text == "nil" then return []
  if !text.startsWith lParen then
    throw s!"listFromString: expected string to start with '{lParen}', got '{text}'"
  if !text.endsWith rParen then
    throw s!"listFromString: expected string to end with '{rParen}', got '{text}'"
  let text := text.drop 1 |>.dropRight 1
  let parts := text.splitButParens ','
  let #[head, tail] := parts |
    throw s!"listFromString: expected string to have two parts, got '{text}'"
  let head := head.trim.toString
  let tail <- listFromString tail.trim.toString
  return head :: tail
  

partial def Node.fromString (text : String) : Except String Node := do
  let lst <- listFromString text
  let head :: tail := lst
    | throw s!"Node.fromString: expected list to have at least one element, got '{text}'"
  return ⟨head, <- tail.mapM Node.fromString⟩


#guard Node.fromString "[ hello, [ [ world, nil ], nil ] ]"
  == .ok ⟨"hello", [⟨"world", []⟩]⟩
