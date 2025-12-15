import Latex2Lean.Latex.Basic
import Latex2Lean.Node.Basic


open NessieParse (ParserM Pos)
open NessieParse.Parser (
  -- charEq
  digit
  expectString
  letter
  oneOf
  skipWhitespace
  whitespace
)


private def ignore {α} (_ : α) : Unit := ()


/-- For some reason, charEq doesn't just always return unit. This fixes this. -/
private abbrev charEq {E F} [Inhabited F] (ch : Char)
: NessieParse.Parser Unit E F :=
  NessieParse.Parser.charEq ch |>.map ignore


namespace Latex


abbrev Name := String


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


inductive Never


def separatedBy {α} (sep : Parser Unit) (p : Parser α (F := Never))
: Parser (List α) (F := Never) :=
  p
  |>.mapFail nofun
  |>.andThen (fun a =>
    sep.andThen (fun _ => p |>.mapFail nofun)
    |>.repeat0
    |>.map (a :: ·)
  )


/--
An atom.

Empty set - \emptyset / \varnothing / \{\}
Brackets - {e} is the same as e
Variables - x when x is in the context
Numbers - 1 2 3
Absolute - \abs A or |A| (Currently |A| is unsupported)
Set - \{ 1, 2, \{ 3 \} \} or \set{ 1, 2, \set{ 3 } }
Advanced sets - \{ x + 1 \mid x \in A \} or \set{ x \in A \mid -x \in A }
-/
partial def atom (expr : Parser Node) : Parser Node := ParserM.run do
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
    -- \set{ ... }
    setWithKeyword (),
    -- (x, y)
    tup (),
  ]
where
  bracketed _ : Parser Node := ParserM.run do
    let lhsCurly <- position
    charEq '{'
    let inner <- expr.orErr (.thereShouldBeAFormulaBetweenCurlyBraces lhsCurly)
    skipWhitespace
    charEq '}' (F := Unit) |>.orErr (.missingRightCurlyBrace lhsCurly)
    return inner
  abs _ : Parser Node :=
    commandEq "abs"
    |>.orFail default
    |>.andThen fun () =>
      position.andThen fun start =>
        atom expr
        |>.orErr (.expectedFormula start) -- TODO
        |>.map fun inner => ⟨"abs", [inner]⟩
  absWithPipes _ : Parser Node := ParserM.run do
    charEq '|'
    let inner <- expr.andThenFail fun _ => panic! "TODO"
    -- BUG: This will never actually reach, because of having the pipe above. We
    -- can either do this with a reader monad and propegating a configuration
    -- that tells us when to disallow pipes for absolute, or we can just have an
    -- atom that contains the piped-abosuluted and one that doesn't
    -- TODO
    charEq '|' (F := Unit) |>.andThenFail fun _ => panic! "TODO"
    return ⟨"abs", [inner]⟩
  emptySet _ : Parser Node := ParserM.run do
    commandEq "{"
    skipWhitespace
    commandEq "}"
    return ⟨"new-set", []⟩
  set _ : Parser Node := ParserM.run do
    commandEq "{"
    let lhs <- expr
    skipWhitespace
    oneOf [
      -- \{ .. \mid .. \}
      ParserM.run do
        commandEq "mid"
        let rhs <- expr
        skipWhitespace
        commandEq "}"
        return ⟨"map", [lhs, rhs]⟩,
      -- \{ a, b, c \}
      ParserM.run do
        let rest <-
          (ParserM.run do
            symbolEq ","
            let r <- expr
            skipWhitespace
            return r
          ).repeat0
        let l := lhs :: rest
        commandEq "}"
        return ⟨"new-set", l⟩,
    ]
  setWithKeyword _ : Parser Node := ParserM.run do
    commandEq "set"
    skipWhitespace
    charEq '{'
    let lhs <- expr
    skipWhitespace
    oneOf [
      -- \set{ .. \mid .. }
      ParserM.run do
        commandEq "mid"
        let rhs <- expr
        skipWhitespace
        charEq '}'
        return ⟨"map", [lhs, rhs]⟩,
      -- \set{ a, b, c }
      ParserM.run do
        let rest <-
          (ParserM.run do
            symbolEq ","
            let r <- expr
            skipWhitespace
            return r
          ).repeat0
        let l := lhs :: rest
        skipWhitespace
        charEq '}'
        return ⟨"new-set", l⟩,
    ]
  tup _ : Parser Node := ParserM.run do
    symbolEq "("
    skipWhitespace
    let children <- oneOf [
      symbolEq ")" |>.map (fun _ => []),
      separatedBy (ParserM.run do
        skipWhitespace
        charEq ','
        skipWhitespace) (expr.andThenFail fun _ => panic! "")
      |>.mapFail nofun
    ]
    return ⟨"tuple", children⟩



/--
An expr.

Supported expression:
Atom - 123, { x + 1 }
Setting a variable - x = \{ 1, 2, 3 \}
Binary expression - x * y (no precedence yet, parenthesis always on the right)
-/
partial def expr (_ : Unit) : Parser Node := ParserM.run do
  skipWhitespace
  oneOf [
    setVar (),
    binaryExpr (),
  ]
where
  setVar _ : Parser Node := ParserM.run do
    let lhs <- name
    skipWhitespace
    charEq '='
    let here <- position
    skipWhitespace
    let rhs <- expr () |>.orErr (.shouldHaveFormulaAfterEq here)
    let lhs := ⟨lhs, []⟩
    return ⟨"=", [lhs, rhs]⟩
  atom' _ := atom (expr ())
  binaryExpr _ : Parser Node := ParserM.run do
    let lhs <- atom' ()
    skipWhitespace
    let op <- binaryOperator.maybe
    match op with
    | some op =>
      let rhs <- expr ()
      return ⟨op, [lhs, rhs]⟩
    | none =>
      return lhs
  /-- symbolEq s but also returns s. -/
  symbolEq' s := symbolEq s |>.map fun () => s
  /-- commandEq s but also returns s. -/
  commandEq' s := commandEq s |>.map fun () => s
  binaryOperator : Parser String := ParserM.run do
    oneOf [
      symbolEq' "+",
      symbolEq' "-",
      symbolEq' "*",
      symbolEq' "/",
      commandEq' "notin",
      commandEq' "in",
      commandEq' "subset",
      commandEq' "subseteq",
      commandEq' "supset",
      commandEq' "supseteq",
      commandEq "cap" |>.map fun _ => "intersect",
      commandEq "cup" |>.map fun _ => "union",
      commandEq' "times" |>.map fun _ => "cross",
    ]


def mathMode : Parser Node := expr ()



def mathModeForSure : Parser Node (F := Never) := ParserM.run do
  let here <- position
  mathMode
  |>.orErr (Error.notStartOfMathModeExpression here)
  -- |>.andThenFail fun .mk =>


/- Some tests
#eval mathMode |>.run (.ofString r"hello")
#eval mathMode |>.run (.ofString r"\{ \}")
#eval mathMode |>.run (.ofString r"\{ q, s, t \}")
#eval mathMode |>.run (.ofString r"|\{ q, s, t \}|")
-/
