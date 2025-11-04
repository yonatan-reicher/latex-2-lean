import Latex2Lean.Assumption.Basic
import Latex2Lean.Assumption.ToAssumptionKind
import Latex2Lean.Latex.Basic
import Latex2Lean.Latex.TextMode
import Latex2Lean.Latex.MathMode


open Lean (TSyntax)


namespace Latex


private def endPos (s : Substring) : Pos :=
  let parser := (NessieParse.Parser.char.map ignore).repeat0
  let initialState := NessieParse.State.ofString s.toString
  let output := parser.parse initialState (F := Never) (E := Never)
  match output with
  | .ok _ p => p
  | .fail f _ => nomatch f
  | .error e _ => nomatch e


partial def textToNodes (text : Substring)
: Except Error (Array Node) := do
  let inners := getMathModeText text
  inners.mapM fun (math, nCharactersBefore) => do
    let beforeMath := text.take nCharactersBefore
    let posAtMathStart := endPos beforeMath
    let actualStartPos := posAtMathStart
    match mathModeForSure.parse (.ofString math.toString) with
    | .ok x _ => return x
    | .fail f _ => nomatch f
    | .error e _ => throw (fixError actualStartPos e)
where 
  fixError addToPosition e :=
    match e with
    | .notStartOfMathModeExpression p =>
      .notStartOfMathModeExpression (addToPosition + p)
    | .shouldHaveFormulaAfterEq p =>
      .shouldHaveFormulaAfterEq (addToPosition + p)
    | .missingRightCurlyBrace p =>
      .missingRightCurlyBrace (addToPosition + p)
    | .thereShouldBeAFormulaBetweenCurlyBraces p =>
      .thereShouldBeAFormulaBetweenCurlyBraces (addToPosition + p)
