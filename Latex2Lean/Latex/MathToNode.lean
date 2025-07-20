import Latex2Lean.Node.Basic
import Latex2Lean.Latex.MathMode


open Lean (Name TSyntax CoreM)


/-- Get the unhygenic String of an identifier -/
private def identToString (ident : TSyntax `ident) : String :=
  ident.getId.eraseMacroScopes.toString


/-- Get the string of a number. -/
private def numToString (num : TSyntax `num) : String :=
  toString num.getNat


-- #print Lean.SyntaxNodeKinds
-- #print Lean.SyntaxNodeKind
private def Lean.TSyntax.category {c: Name} (_: TSyntax [c]) : Name := c


instance : MonadLift (Except BadLatex) Lean.CoreM where
  monadLift except :=
    match except with
    | .ok x => return x
    | .error (.math e) => do
      let f <- Lean.PrettyPrinter.ppCategory e.category e
      throwError s!"The syntax '{f}' is unsupported"


/--
Takes LaTeX math syntax as input, and returns the correct node or whatever
-/
partial def LatexMath.toNode (x : LatexMath) : Except BadLatex Node :=
  match x with
  | `(latexMath| $var:ident) => return ⟨identToString var, []⟩
  | `(latexMath| $n:num) => return ⟨numToString n, []⟩
  | `(latexMath| \abs $set) => return ⟨"abs", [<- toNode set]⟩
  | `(latexMath| {$x}) => toNode x
  | `(latexMath| \{ $args,* \}) => do
    let nodes <- args.getElems.mapM toNode
    return ⟨"new-set", nodes.toList⟩
  | `(latexMath| $lhs $op:latexMathBinOp $rhs) => do
    -- TODO: Make this a different error
    let some op := opStr op | throw (.math x)
    return ⟨op, [<- toNode lhs, <- toNode rhs]⟩
  | _ => throw (.math x)
where
  opStr
    | `(latexMathBinOp|+) => some "+"
    | `(latexMathBinOp|-) => some "-"
    | `(latexMathBinOp|=) => some "="
    | `(latexMathBinOp|\cap) => some "intersect"
    | `(latexMathBinOp|\cup) => some "union"
    | _ => none


#eval show CoreM _ from do LatexMath.toNode (<- `(latexMath|\{\}))
#eval show CoreM _ from do liftM $ LatexMath.toNode (<- `(latexMath|124))
#eval show CoreM _ from do liftM $ LatexMath.toNode (<- `(latexMath|hello))
#eval show CoreM _ from do liftM $ LatexMath.toNode (<- `(latexMath|hello))
-- #eval show CoreM _ from do liftM $ LatexMath.toNode (<- `(latexMath|\{ x \in 1 \mid 1 \}))

