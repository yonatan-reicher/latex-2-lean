from z3 import *
z3.set_param("smt.random_seed", 3512351)
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
s.add(Sublist(l1, l2))
s.add(ListSingleton(a, a_singleton))
s.add(ListConcat(l1, a_singleton, l3))
s.add(ForAll([x], Implies(l1.contains(x), x < a)))
s.add(l2.contains(a))
s.add(ListSorted(l2))
s.add(*l1.guards(), *l2.guards(), *l3.guards())
x = NotSublist(l3, l2)
print(x)
x = simplify(x)
print(x)
s.add(x)
s.set("timeout", 1000)

iter = 0
while True:
    iter += 1
    print(f"Trying lists of len < {iter}")
    s.push()
    s.add(l1.len < iter, l2.len < iter, l3.len < iter)
    r = s.check()
    print(r)
    print(s.model() if r == sat else None)
    s.pop()
    input()

# print(f"l3 {l3 @ m}")
