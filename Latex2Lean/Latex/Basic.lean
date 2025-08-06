import NessieParse


open NessieParse.Parser (CombineFail CombineManyFail)


inductive Error
| thereShouldBeAFormulaBetweenCurlyBraces
| missingRightCurlyBrace
deriving DecidableEq, Hashable, Inhabited, Repr


structure Failure
deriving DecidableEq, Hashable, Inhabited, Repr
instance : CombineFail Failure Failure Failure where
  combine _ _ := .mk
instance : CombineManyFail Failure Failure where
  combineMany _ := .mk


abbrev Parser α (E := Error) (F := Failure) := NessieParse.Parser α E F
