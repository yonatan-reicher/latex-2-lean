# Latex2Lean

This project is an attempt in automatically converting LaTeX code and formulae
directly to Lean4, in a way that can integrate with your editor.

Basically, this library provides you with a command that you can use to read
LaTeX from a file, and it will be seamlessly added to the environment, as
regular Lean definitions. Specifically, the goal is to add the definitions,
letting you explore them formally inside of Lean (translating whole proofs does
not seem feasible)
