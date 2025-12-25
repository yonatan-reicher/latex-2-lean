import Latex2Lean.InlineMath


namespace Latex2Lean.Input


def file (path : System.FilePath) : IO (Array Char) := do
  let str <- IO.FS.readFile path
  return str.toList.toArray


def str (s : String) : Array Char :=
  s.toList.toArray
