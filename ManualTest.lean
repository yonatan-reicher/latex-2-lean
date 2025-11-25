import Latex2Lean


-- For some reason, no messages are printed? this is weird, because the given
-- file name does not refer to an actual file! so an exception is definitly
-- thrown inside. It's just not printed??
define_latex file "proof"

#eval defineLatexFromFile "proof-adjusted.md"
