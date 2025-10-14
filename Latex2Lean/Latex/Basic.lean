import NessieParse


open NessieParse.Parser (CombineFail CombineManyFail)


namespace Latex


inductive Error
| thereShouldBeAFormulaBetweenCurlyBraces
| missingRightCurlyBrace
| shouldHaveFormulaAfterEq
deriving DecidableEq, Hashable, Inhabited, Repr


instance : ToString Error where
  toString e := toString $ repr e


structure Failure
deriving DecidableEq, Hashable, Inhabited, Repr
instance : CombineFail Failure Failure Failure where
  combine _ _ := .mk
instance : CombineManyFail Failure Failure where
  combineMany _ := .mk


abbrev Parser α (E := Error) (F := Failure) := NessieParse.Parser α E F


abbrev MathModeText := Substring
