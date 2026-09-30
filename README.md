# Latex To Lean

This project is an attempt in automatically converting some subset of LaTeX code
to Lean 4.

The project is basically a library that provides a macro that takes some text,
parses it, analyses it, and then emits some Lean code into the environment with
it.

## High Level

> [!info]
> Inside Lean, the syntax is:
> ```
> define_latex file [verbose] "<file-path>"
> define_latex      [verbose] r"<source-text>"
> ```

(Note that `r".."` is Lean syntax for raw strings, allowing you to write "\sum"
without the backslash interpreted as the start of an escape sequence. These
strings also accept raw newline characters)

## Low Level

Please be familiar with the following files:

### Files

#### Documentation

- `CHANGELOG.md` - what I haven't shown Shachar yet
- `CLAUDE.md`
- `README.md`
- `design.md` - original idea for the main pipeline
- `grammar.txt` - grammar for the parser, not exactly accurate

#### Markdown Proofs

- `ladder.md` - attempt at translating part of the multi-segment project's proofs
- `proof-adjusted.md` - yuval's domino proof, adjusted to be readable by the tool
- `proof.md`- yuval's original domino proof

#### Lean Source

The most import file is `ManualTest.lean`, where you can test things out.
After that, it's probably the ones relating directly to the main pipeline, and
then things relating to the AST.

##### Pipeline

- `Latex2Lean/Parsing.lean`
- `Latex2Lean/Categorizing.lean`
- `Latex2Lean/Analysing.lean`
- `Latex2Lean/Translating.lean`

##### AST

- `Latex2Lean/Formula.lean`
- `Latex2Lean/CategorizedFormula.lean`
- `Latex2Lean/LeanCmd.lean.lean`

#### Lean Tests

There are `#guard` and `#guard_msgs` in the source files, but there is also the
`Latex2LeanTests` directory.

- `Latex2LeanTests/`
- `├── Application.lean`
- `├── Exists.lean`
- `├── Finset.lean`
- `├── Forall.lean`
- `└── Sum.lean`

#### Source Generation

- `binary_operators.table`

#### Analysis

- `analysis/` - the current version of the static analysis
- `souffle-analysis/` - old version of the static analysis
- `ui/` - Shachar did some node something for the counter example test, i
  haven't looked at this
- `venv/` - the main virtual environment (there is another one somewhere, but
  that is old, use `. venv/bin/activate` on bash, and
  `overlay use venv/bin/activate.nu` on Nushell)

#### Misc

- `zfc-abstractions/` - playing around with making a another tool that finds
  satisfying models

## Next Steps

Fix analysis ast kind
Update the scoping rules.
Deal with the latest git stash.
Set comprehension - parse input and update rules.
Remove souffle code.

## On Adding Custom Notation

Thinking about how to add custom notation. We said that we should start by just
supporting custom notation for binary operators. In our example, we had:
$a \in^2 b = \exists c, a \in c \land c \in b$. There are a couple of things we
need to consider about this. What would be very cool is if this was parsed as an
axiom formula, and the axiom would introduce the missing operator by itself.
That seems far fetched. Maybe something better would be to just tag it and make
custom syntax.

Simplest syntax to make our life easiest:
$a \in^2 b: \exists c, a \in c \land c \in b$. Even simpler, because we don't
support \land's and \exists', we could just define it as
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
