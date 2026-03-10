# Latex2Lean

This project is an attempt in automatically converting LaTeX code and formulae
directly to Lean4, in a way that can integrate with your editor.

Basically, this library provides you with a command that you can use to read
LaTeX from a file, and it will be seamlessly added to the environment, as
regular Lean definitions. Specifically, the goal is to add the definitions,
letting you explore them formally inside of Lean (translating whole proofs does
not seem feasible)

## Currently Working On

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

## TODO

In axioms, introduce variables if they don't exist
Make things types instead
Abbreviations (Custom notation)
Sum function
