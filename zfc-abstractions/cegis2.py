import z3
import z3_monkey
from dataclasses import dataclass
from typing import Callable
from building_blocks.list import List, ListSingleton, Sublist, ListSorted, ListConcat, Nth
from building_blocks.props import Exists, ForAll

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


# S = z3.DeclareSort('S')
S = z3.IntSort()
x, y, z, a = z3.Consts('x y z a', S)
l1, l2, l3 = List('l₁', S), List('l₂', S), List('l₃', S)
a_singleton = List('a_singleton', S)
s = z3.Solver()
s.add(
    Sublist(l1, l2),
    ListSingleton(a, a_singleton),
    ListConcat(l1, a_singleton, l3),
    ForAll([x], z3.Implies(l1.contains(x), x <= a)),
    l2.contains(a),
    ListSorted(l2),
    *l1.guards(), *l2.guards(), *l3.guards(),
)
s.set("timeout", 1000)
def badness(candidate: z3.ModelRef):
    """ bad if l3 is a sublist of l2 """
    l3_, l2_ = (l @ candidate for l in (l3, l2))
    if len(l3_) > len(l2_): return None
    mapping = {}
    i_l3 = 0
    for i_l2, x_l2 in enumerate(l2_):
        if i_l3 >= len(l3_): break
        x_l3 = l3_[i_l3]
        if x_l2 == x_l3:
            mapping[i_l3] = i_l2
            i_l3 += 1
    is_sublist = i_l3 == len(l3_)
    if not is_sublist: return None
    return z3.Implies(
        l3.len == len(l3_),
        z3.Not(z3.And(
            Nth(l3[i], j, l2)
            for i, j in mapping.items()
        )),
    )
for n in range(100):
    s.push()
    s.add(l1.len < n, l2.len < n, l3.len < n)
    c = Cegis(s, badness)
    r = c.solve()
    print(r)
    if isinstance(r, z3.ModelRef):
        print(l1 @ r, l2 @ r, l3 @ r)
    input()
    s.pop()
