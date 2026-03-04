import Lean

/-!
Utilities for `Lean` (the imported library, not the language :P)
-/

open Lean

instance : ToMessageData Exception where
  toMessageData := Exception.toMessageData
