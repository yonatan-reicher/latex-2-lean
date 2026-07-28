import z3
import z3
from dataclasses import dataclass
from typing import Callable


@dataclass(frozen=True, slots=True)
class CannotSolve:
    reason: str


Condition = Callable[[z3.ModelRef], z3.BoolRef | CannotSolve | None]


class Cegis:
    solver: z3.Solver
    badness: Condition

    def __init__(self, solver: z3.Solver, badness: Condition):
        self.solver = solver
        self.badness = badness

    def _get_candidate(self) -> z3.ModelRef | CannotSolve | None:
        r = self.solver.check()
        if r == z3.sat:
            return self.solver.model()
        elif r == z3.unknown:
            return CannotSolve(self.solver.reason_unknown())
        else:
            assert r == z3.unsat
            return None

    def solve(self) -> z3.ModelRef | CannotSolve | None:
        while True:
            r = self._get_candidate()
            if r is None:
                return None
            if isinstance(r, CannotSolve):
                return r
            candidate = r
            print("Found candidate", candidate)
            r = self.badness(candidate)
            if r is None:
                return candidate
            if isinstance(r, CannotSolve):
                return r
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
a = z3.Array('a', z3.IntSort(), S)

# Candidate generator: same style as cegis2.py
s = z3.Solver()
s.add([z3.Exists([i], v == a[i]) for v in [x, y, z]])
s.set('timeout', 1000)


def badness(candidate: z3.ModelRef, *, sample=128):
    """
    Attempt to generalize with Spacer over a sampled window. If Spacer proves
    there is no adjacent equality in the sampled window, return None. Otherwise
    fall back to returning conjunctive ground conflicts mined from the sample.
    """
    # First, try a lightweight Spacer attempt on a finite sample of indices.
    fp = z3.Fixedpoint()
    fp.set(engine='spacer')

    Seed = z3.Function('Seed', z3.BoolSort())
    Arr = z3.Function('Arr', z3.IntSort(), S, z3.BoolSort())
    Bad = z3.Function('Bad', z3.BoolSort())
    fp.register_relation(Seed, Arr, Bad)

    # declare rule vars (ii for indices, v_s for S-sorted values)
    ii = z3.Int('ii')
    v_s = z3.Const('v_s', S)
    fp.declare_var(ii, v_s)

    fp.rule(Seed())

    # Seed Arr facts from the candidate model for indices in [0..sample-1]
    for k in range(sample):
        val = candidate.eval(a[k], model_completion=True)
        fp.rule(Arr(z3.IntVal(k), val))

    # Bad holds if there exists j in sampled window with equal neighbors
    jv = z3.Int('jv')
    fp.declare_var(jv)
    fp.rule(Bad(), [Seed(), 0 <= jv, jv < sample - 1, Arr(jv, v_s), Arr(jv + 1, v_s)])

    try:
        q = fp.query(Bad())
    except z3.Z3Exception as e:
        return CannotSolve(str(e))

    if q == z3.unsat:
        # No bad adjacent pair found in sampled window under this candidate.
        return None

    # Otherwise, collect all equal neighbor indices in the sample and block them.
    conflicts = []
    for k in range(sample - 1):
        v0 = candidate.eval(a[k], model_completion=True)
        v1 = candidate.eval(a[k + 1], model_completion=True)
        if z3.eq(v0, v1):
            conflicts.append(a[k] != a[k + 1])

    if not conflicts:
        # As a fallback, return a single ground block using the first sample index.
        return a[0] != a[1]
    return z3.And(conflicts)


if __name__ == '__main__':
    c = Cegis(s, badness)
    print(c.solve())
