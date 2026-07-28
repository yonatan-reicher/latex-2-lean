# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Latex2Lean** converts LaTeX mathematical notation into Lean 4 definitions. The user-facing macro `define_latex` processes LaTeX strings or files and registers the resulting definitions in the Lean environment.

## Build Commands

```bash
lake build          # Build the library
lake env lean ManualTest.lean  # Run the manual test file
```

Testing is done via `#guard` assertions embedded in source files (Lean's built-in test mechanism) and via `ManualTest.lean` which exercises the `define_latex` macro end-to-end.

## Pipeline Architecture

The system is an 8-stage pipeline defined in `design.md`:

```
Input text
  → Spanning     (Spanning.lean)      extract $...$ math spans
  → Lexing       (Lexing.lean)        tokens
  → Parsing      (Parsing.lean)       Formula AST
  → Categorizing (Categorizing.lean)  definition / axiom / plain
  → Analysing    (Analysing.lean)     static analysis via Souffle datalog
  → Translating  (Translating.lean)   Lean expressions (LeanCmd)
  → Emitting     (Emitting.lean)      register in Lean environment
```

Entry point: `Functions.lean` (`defineLatex`), exposed as the `define_latex` macro in `Macros.lean`.

## Key Source Files

| File | Role |
|---|---|
| `Latex2Lean/Functions.lean` | High-level API; orchestrates the pipeline |
| `Latex2Lean/Macros.lean` | User-facing `define_latex` macro |
| `Latex2Lean/Lexing.lean` | Tokenizer |
| `Latex2Lean/Parsing.lean` | Recursive descent parser → `Formula` AST |
| `Latex2Lean/Categorizing.lean` | Classifies formulas as definition/axiom/plain |
| `Latex2Lean/Analysing.lean` | Orchestrates Souffle static analysis |
| `Latex2Lean/Translating.lean` | `Formula` → Lean `Expr` / `LeanCmd` |
| `Latex2Lean/Emitting.lean` | Adds `LeanCmd` values to the Lean environment |
| `Latex2Lean/Formula.lean` | Core AST type |
| `Latex2Lean/Token.lean` | Token type |
| `ManualTest.lean` | End-to-end usage examples |

## Core Data Types

**`Formula`** — the AST produced by parsing:
- `emptySet`, `var`, `number`, `abs`, `binOp`, `simpleSet`, `mapSet`, `tuple`
- `BinOp`: `plus`, `minus`, `star`, `slash`, `eq`, `cap` (∩), `cup` (∪), `in_` (∈)

**`Token`** — output of lexing, carries `Token.Kind` and a `Range` (source position).

**`LeanCmd`** — either `def_ name expr` or `axiom_ expr`; consumed by Emitting.

**`Analysis`** — reader context providing `isFiniteSet` and `mustBeFiniteSet` HashSets, produced by running Souffle on CSV exports.

## Translation Stage Notes

`Translating.lean` is the most complex stage. It uses `TermElabM`/`MetaM` to produce Lean expressions. Key helpers:
- `asNumber` — translates a formula as a natural number (handles `\abs` as `Finset.card`)
- `asFinset` — translates a formula as a `Finset`
- `asTuple` — translates a formula as a product type tuple
- `asExpr` — general dispatcher; uses `Analysis` to pick the right representation

The `Analysis` context (from Souffle) determines whether a subexpression should be treated as a `Finset` or a general `Set`.

## Supported LaTeX Constructs

- Sets: `\set{1, 2, 3}`, `{1, 2, 3}`, `\emptyset`, `\varnothing`
- Set comprehension: `\set{x | x \in S}`
- Operators: `+`, `-`, `*`, `/`, `=`, `\cap`, `\cup`, `\in`
- Absolute value / cardinality: `\abs x`
- Ranges: `1..10`
- Tuples: `(a, b, c)`

## Dependencies

- `mathlib` — Lean 4 math library
- `NessieParse` — parsing utilities
- `Souffle` — external Datalog engine used for static analysis (CSV I/O via `Analysis/FromCsvs.lean`)
