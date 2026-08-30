# Copilot Instructions

## Build & Test

```bash
lake build                         # Build the library
lake env lean ManualTest.lean      # Run end-to-end manual tests
```

Tests are written as `#guard` assertions inline in source files and `#guard_msgs` blocks (which also verify expected output). There is no separate test runner — `lake build` will check all `#guard` assertions.

## Architecture

Latex2Lean is an 8-stage pipeline that converts LaTeX math notation into Lean 4 definitions and axioms. The user-facing entry point is the `define_latex` macro (`Macros.lean`), which calls `defineLatex` in `Functions.lean`.

**Pipeline stages** (each has a corresponding `.lean` file):
```
Input text
  → Spanning      extract $...$ / $$...$$ math spans
  → Lexing        Array Token
  → Parsing       Formula AST
  → Categorizing  CategorizedFormula (definition / axiom / plain)
  → Analysing     runs Souffle Datalog for static analysis → Analysis
  → Translating   Formula → LeanCmd (def_ / axiom_)
  → Emitting      registers LeanCmd in the Lean environment via addDecl
```

The `Analysis` struct (produced by the Souffle stage) carries `isFiniteSet` and `mustBeFiniteSet` HashSets. These are passed as a `ReaderT` context through `Translating.lean` and determine whether a set-valued expression becomes a `Finset` or a `Set`.

## Core Types

- **`Formula`** — the AST. Variants: `emptySet`, `var`, `number`, `abs`, `binOp`, `simpleSet`, `mapSet`, `tuple`. Always carries a `Range` as last field.
- **`BinOp`** — `plus | minus | star | slash | eq | cap | cup | in_`
- **`Token`** / **`Token.Kind`** — lexer output, carries a `Range`.
- **`CategorizedFormula`** — `definition name range formula | axiom_ formula | plain formula`
- **`LeanCmd`** — `def_ name expr | axiom_ expr`; consumed by `Emitting.lean`.
- **`Analysis`** — reader context for the translation stage.

## Key Conventions

**Monad stacks**: Each pipeline stage defines its own monad alias:
- `Parsing`: `M = StateT (Subarray Token) (ExceptT Error Id)` with `T m` for the parametric variant.
- `Translating`: `M = ReaderT Analysis TermElabM`

**`..` pattern in constructors**: Formula constructors end with a `Range` field. The `..` syntax is used in pattern matches to ignore it, e.g. `| .var name .. =>`.

**Inline `#guard` tests**: Unit tests are placed directly in source files as `#guard expr` or `#guard_msgs in #eval ...`. The expected output is written as a doc-comment above `#guard_msgs`, e.g.:
```lean
/-- info: true -/
#guard_msgs in #eval someFunction args
```

**`autoImplicit = false`** is set globally in `lakefile.toml` — all variables must be explicitly declared.

**Souffle integration**: Static analysis is invoked via `Souffle.call` in `Analysing.lean`, which writes CSV files (`assumption.csv`, `expr.csv`) and reads back results. The `Node` type is an intermediate tree serialized to CSV strings for Souffle.

**`define_latex` macro variants**:
```lean
define_latex "inline LaTeX string"
define_latex file "path/to/file.md"
define_latex verbose "..."          -- logs emitted commands
define_latex file verbose "..."
```

## Dependencies

- `mathlib` — Lean 4 math library (provides `Finset`, `Set`, etc.)
- `NessieParse` — parsing utilities (from `github.com/yonatan-reicher/lean-nessie-parse`)
- `Souffle` — external Datalog engine for static analysis
