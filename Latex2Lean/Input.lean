import Latex2Lean.InlineMath


namespace Latex2Lean

class Input α where
  getInput : α → IO (Array Char)


instance : Input (Array Char) where
  getInput := pure


instance : Input String where
  getInput s := pure s.toList.toArray


instance : Input System.FilePath where
  getInput path := do
    let str <- IO.FS.readFile path
    return str.toList.toArray
