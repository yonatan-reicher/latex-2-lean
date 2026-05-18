import z3
from dataclasses import dataclass
from typing import Callable

@dataclass(frozen=True, slots=True)
class CannotSolve:
    reason: str

Condition = Callable[[z3.ModelRef], z3.BoolRef | CannotSolve | None]
ConflictClause = z3.BoolRef
""" A clause that blocks the badness condition from being satisfied, for some
candidate."""

class Cegis:
    solver: z3.Solver
    badness: Condition

    def __init__(self, solver: z3.Solver, badness: Condition):
        self.solver = solver
        self.badness = badness

    def _get_candidate(self) -> z3.ModelRef | CannotSolve | None:
        """
        Get the next satisfying assignments for the constraints, except for
        the badness condition.
        """
        r = self.solver.check()
        if r == z3.sat:
            return self.solver.model()
        elif r == z3.unknown:
            return CannotSolve(self.solver.reason_unknown())
        else:
            assert r == z3.unsat
            return None

    def _block_term(self, m, t):
        """ Make sure a term is different from the value in the model. """
        self.solver.add(t != m.eval(t, model_completion=True))

    def _fix_term(self, m, t):
        """ Make sure a term has the exact same value as in the model. """
        self.solver.add(t == m.eval(t, model_completion=True))

    def solve(self) -> z3.ModelRef | CannotSolve | None:
        while True:
            r = self._get_candidate()
            if r is None: return None
            elif isinstance(r, CannotSolve): return r
            candidate = r
            print("Found candidate", candidate)
            r = self.badness(candidate)
            if r is None: return candidate
            elif isinstance(r, CannotSolve): return r
            conflict_clause = r
            print("Found conflict", conflict_clause)
            self.solver.add(conflict_clause)


def diagram(m: z3.ModelRef) -> dict[z3.ExprRef, z3.ExprRef]:
    return {
        d(): m.eval(d(), model_completion=True)
        for d in m.decls() if d.arity() == 0
    }


S = z3.DeclareSort('S')
x, y, z = z3.Consts('x y z', S)
i, j = z3.Ints('i j')
a, b = z3.Array('a', z3.IntSort(), S), z3.Array('b', z3.IntSort(), S)
s = z3.Solver()
s.add([z3.Exists([i], v == a[i]) for v in [x, y, z]])
s.set("timeout", 1000)
def badness(candidate: z3.ModelRef):
    i = z3.Int('i')
    s = z3.Solver()
    s.add(*(var == value for var, value in diagram(candidate).items()))
    s.add(a[i] == a[i + 1])
    s.add(0 <= i, i < 100)
    r = s.check()
    if r == z3.sat:
        i = s.model().eval(i, model_completion=True).py_value()
        assert isinstance(i, int)
        return a[i] != a[i + 1]
    elif r == z3.unsat:
        return None
    else:
        assert r == z3.unknown
        return CannotSolve(s.reason_unknown())
c = Cegis(s, badness)
print(c.solve())
