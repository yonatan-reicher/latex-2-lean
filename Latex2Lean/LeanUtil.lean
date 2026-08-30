module

public import Lean

public section


/-!
Utilities for `Lean` (the imported library, not the language :P)
-/

open Lean
open Lean.Meta

instance : ToMessageData Exception where
  toMessageData := Exception.toMessageData