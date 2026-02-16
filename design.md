What we are doing: Take text and make Lean definitions.

The process:

| # | Stage         | Output        | Representation            | Extra Info   |
| - | ------------- | ------------- | ------------------------- | ------------ |
| 1 | Input         | Text          | Array (Array Char)        |              |
| 2 | Spanning      | Inline Math   | Array (Kind ✗ Span)[^1]   |              |
| 3 | Lexing        | Tokens        | Array (Kind ✗ List Token) |              |
| 4 | Parsing       | Ast           | Array (Kind ✗ Formula)    |              |
| 5 | Categorizing  | Category      | Array CategorizedFormula  |              |
| 6 | Analysing     | Analysis      | Analysis ✗ Array ...      |              |
| 7 | Translation   | Lean Commands | Array LeanCmd             |              |
| 8 | Emitting      | Lean Commands | CommandElabM Unit         |              |
[^1]: (Span = Nat⁴ ✗ Array Char)

