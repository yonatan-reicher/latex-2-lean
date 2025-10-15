import NessieParse


open NessieParse.Parser (CombineFail CombineManyFail)


namespace Latex


def Pos := NessieParse.Pos
deriving DecidableEq, Hashable, Inhabited


instance : Repr Pos where
  reprPrec pos _ := 
    s!"[{pos.row}:{pos.col}]"


inductive Error
| thereShouldBeAFormulaBetweenCurlyBraces (lhs /- rhs -/ : Pos)
| missingRightCurlyBrace (start : Pos)
| shouldHaveFormulaAfterEq (here : Pos)
| notStartOfMathModeExpression (start /- _end -/ : Pos)
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


def position {e f} : NessieParse.Parser Pos e f :=
  NessieParse.Parser.state.map (·.pos)
