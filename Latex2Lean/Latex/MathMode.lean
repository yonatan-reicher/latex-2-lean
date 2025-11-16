import Latex2Lean.Latex.Basic
import Latex2Lean.Node.Basic


open NessieParse (ParserM Pos)
open NessieParse.Parser (
  charEq
  digit
  expectString
  letter
  oneOf
  skipWhitespace
  whitespace
)


namespace Latex


abbrev Name := String


def ignore {α} (_ : α) : Unit := ()


def name : Parser Name :=
  letter.repeat1
  |>.orFail default
  |>.map String.mk


def number : Parser String :=
  digit.repeat1
  |>.orFail default
  |>.map String.mk


def symbolEq (s : String) : Parser Unit :=
  expectString s |>.orFail default


def commandStart : Parser Unit :=
  charEq '\\'
  |>.map fun _ => ()


def command : Parser Name := ParserM.run do
  commandStart
  name


def commandEq (name : Name) : Parser Unit :=
  assert! name.all (!·.isWhitespace)
  if name.all (·.isAlpha) then
    command.filter (. == name) |>.map ignore
  else
    commandStart.andThen λ() => symbolEq name


partial def mathMode (_ : Unit) : Parser Node := ParserM.run do
  skipWhitespace
  oneOf [
    -- emptyset
    commandEq "emptyset" |>.map (fun () => ⟨"new-set", []⟩)
    |>.orFail default,
    -- varnothing
    commandEq "varnothing" |>.map (fun () => ⟨"new-set", []⟩)
    |>.orFail default,
    -- {1 + 2}
    bracketed (),
    -- x = 2
    setVar (),
    -- x
    name.map (⟨·, []⟩),
    -- 42
    number.map (⟨·, []⟩),
    -- \αbs A
    abs (),
    -- | A |
    absWithPipes (),
    -- \{ \}
    emptySet (),
    -- \{ ... \}
    set (),
  ]
where
  setVar _ : Parser Node := ParserM.run do
    let lhs <- name
    skipWhitespace
    charEq '=' |>.map ignore
    let here <- position
    skipWhitespace
    let rhs <- mathMode () |>.orErr (.shouldHaveFormulaAfterEq here)
    let lhs := ⟨lhs, []⟩
    return ⟨"=", [lhs, rhs]⟩
  bracketed _ : Parser Node := ParserM.run do
    let lhsCurly <- position
    charEq '{' |>.map ignore
    let inner <- mathMode () |>.orErr (.thereShouldBeAFormulaBetweenCurlyBraces lhsCurly)
    skipWhitespace
    charEq '}' (F := Unit) |>.map ignore |>.orErr (.missingRightCurlyBrace lhsCurly)
    return inner
  abs _ : Parser Node :=
    commandEq "abs"
    |>.orFail default
    |>.andThen fun () =>
      mathMode ()
      |>.orErr (panic! "aaaa") -- TODO
      |>.map fun inner => ⟨"abs", [inner]⟩
  absWithPipes _ : Parser Node := ParserM.run do
    charEq '|' |>.map ignore
    let inner <- mathMode () |>.andThenFail fun _ => panic! "TODO"
    charEq '|' (F := Unit) |>.map ignore |>.andThenFail fun _ => panic! "TODO"
    return ⟨"abs", [inner]⟩
  emptySet _ : Parser Node := ParserM.run do
    commandEq "{"
    skipWhitespace
    commandEq "}"
    return ⟨"new-set", []⟩
  set _ : Parser Node := ParserM.run do
    commandEq "{"
    let lhs <- mathMode ()
    skipWhitespace
    oneOf [
      -- \{ .. \mid .. \}
      ParserM.run do
        commandEq "mid"
        let _rhs := mathMode ()
        commandEq "}"
        panic! "TODO", -- TODO
      -- \{ a, b, c \}
      ParserM.run do
        let rest <-
          (ParserM.run do
            symbolEq ","
            let r <- mathMode ()
            skipWhitespace
            return r
          ).repeat0
        let l := lhs :: rest
        commandEq "}"
        return ⟨"new-set", l⟩,
    ]


inductive Never
def mathModeForSure : Parser Node (F := Never) := ParserM.run do
  let here <- position
  mathMode ()
  |>.orErr (Error.notStartOfMathModeExpression here)
  -- |>.andThenFail fun .mk =>


#eval mathMode () |>.run (.ofString r"hello")
#eval mathMode () |>.run (.ofString r"\{ \}")
#eval mathMode () |>.run (.ofString r"\{ q, s, t \}")
#eval mathMode () |>.run (.ofString r"|\{ q, s, t \}|")
