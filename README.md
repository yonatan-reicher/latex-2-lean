# Latex To Lean

This project is an attempt in automatically converting some subset of LaTeX code
to Lean 4.

The project is basically a library that provides a macro that takes some text,
parses it, analyses it, and then emits some Lean code into the environment with
it.

## Files

### Documentation

- `CHANGELOG.md` - what I haven't shown Shachar yet
- `CLAUDE.md`
- `README.md`
- `design.md` - original idea for the main pipeline
- `grammar.txt` - grammar for the parser, not exactly accurate

### Markdown Proofs

- `ladder.md` - attempt at translating part of the multi-segment project's proofs
- `proof-adjusted.md` - yuval's domino proof, adjusted to be readable by the tool
- `proof.md`- yuval's original domino proof

### Lean

- `Latex2Lean.lean`
- `Latex2Lean/`
- `├── Analysing.lean`
- `├── Analysis`
- `│   ├── Basic.lean`
- `│   ├── FromCsvs.lean`
- `│   └── Monad.lean`
- `├── Analysis.lean`
- `├── Basic.lean`
- `├── BinOp.lean`
- `├── CategorizedFormula.lean`
- `├── Categorizing.lean`
- `├── Csv.lean`
- `├── Emitting.lean`
- `├── Formula.lean`
- `├── Functions.lean`
- `├── InlineMath.lean`
- `├── Input.lean`
- `├── LeanCmd.lean`
- `├── LeanUtil.lean`
- `├── Lexing.lean`
- `├── Macros.lean`
- `├── Node`
- `│   ├── Asserts.lean`
- `│   ├── Basic.lean`
- `│   ├── FromLatex.lean`
- `│   ├── FromString.lean`
- `│   ├── ToString.lean`
- `│   └── ToTerm.lean`
- `├── Node.lean`
- `├── Parsing.lean`
- `├── Pos.lean`
- `├── Range.lean`
- `├── RunAnalysisProcess.lean`
- `├── Souffle.lean`
- `├── Spanning.lean`
- `├── Text.lean`
- `├── Token.lean`
- `├── Translating.lean`
- `└── Util.lean`
- `Latex2LeanTests/`
- `├── Application.lean`
- `├── Exists.lean`
- `├── Finset.lean`
- `├── Forall.lean`
- `└── Sum.lean`
1 directory, 5 files
- `ManualTest.lean` - manual testing to see if things works

### Source Generation

- `binary_operators.table`

### Analysis

- `analysis/` - the current version of the static analysis
- `souffle-analysis/` - old version of the static analysis
- `ui/` - Shachar did some node something for the counter example test, i
  haven't looked at this
- `venv/` - the main virtual environment (there is another one somewhere, but
  that is old, use `. venv/bin/activate` on bash, and
  `overlay use venv/bin/activate.nu` on Nushell)

### Misc

- `zfc-abstractions/` - playing around with making a another tool that finds
  satisfying models








# Latex2Lean

This project is an attempt in automatically converting LaTeX code and formulae
directly to Lean4, in a way that can integrate with your editor.

Basically, this library provides you with a command that you can use to read
LaTeX from a file, and it will be seamlessly added to the environment, as
regular Lean definitions. Specifically, the goal is to add the definitions,
letting you explore them formally inside of Lean (translating whole proofs does
not seem feasible)

## Next Steps

Fix analysis ast kind
Update the scoping rules.
Deal with the latest git stash.
Set comprehension - parse input and update rules.
Remove souffle code.

## On Adding Custom Notation

Thinking about how to add custom notation. We said that we should start by just
supporting custom notation for binary operators. In our example, we had:
$a \in^2 b = \exists c, a \in c \and c \in b$. There are a couple of things we
need to consider about this. What would be very cool is if this was parsed as an
axiom formula, and the axiom would introduce the missing operator by itself.
That seems far fetched. Maybe something better would be to just tag it and make
custom syntax.

Simplest syntax to make our life easiest:
$a \in^2 b: \exists c, a \in c \and c \in b$. Even simpler, because we don't
support \and's and \exists', we could just define it as
:a \in^2 b: a \in \set{ a \mid c \in b }

## Maybe One Day

In axioms, introduce variables if they don't exist
Make things types instead
Abbreviations (Custom notation)
Sum function
What to do about mappings with filters that are more than membership? maybe
special case exists?
Add an \exists expression
In setInsides, allow either binders of predicates
