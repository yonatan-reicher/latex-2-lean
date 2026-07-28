from z3 import *
z3.set_param("smt.random_seed", 0)
z3.set_param("proof", True)
from building_blocks.fin import Fin
from building_blocks.set import *
from building_blocks.mapping import *
from building_blocks.props import *
from building_blocks.list import *
import z3_monkey
z3_monkey.mk_mapping = MappingSort

x, y, l1, l2, l3 = Int('x'), Int('y'), List('l₁', IntSort()), List('l₂', IntSort()), List('l₃', IntSort())
a, a_singleton = Int('a'), List('a_singleton', IntSort())

bad = Sublist(l3, l2)
f = Mapping('f', l3.dom ** l2.dom)
bad_sk = substitute_vars(bad.body(), f)

def diag(m: ModelRef):
    return (d() == m.get_interp(d()) for d in m.decls() if d.arity() == 0)

s = Solver()
# s.add(l1.len == 0, l2.len == 2)
# s.add(l1.contains(x))
# s.add(l1.contains(y))
# s.add(x != y)
# s.add(Sublist(l1, l2), Not(Sublist(l2, l1)))
s.add(Sublist(l1, l2))
s.add(ListSingleton(a, a_singleton))
s.add(ListConcat(l1, a_singleton, l3))
s.add(ForAll([x], Implies(l1.contains(x), x <= a)))
s.add(l2.contains(a))
s.add(*l1.guards(), *l2.guards(), *l3.guards())
s.add(l1.len < 100, l2.len < 100, l3.len < 100)
s.add(l1.len == 1, l1[0] == 1)
s.add(l2.len == 3, l2[0] == 14, l2[1] == 1, l2[2] == 1)
s.add(l3.len == 2, l3[0] == 1, l3[1] == 1)
assert s.check() == sat

s_aux = Solver()
s_aux.add(*diag(s.model()))
s_aux.add(bad_sk, *f.guards())
s_aux.set("timeout", 1000)
print(s_aux.check() == unsat)
print(s_aux)
