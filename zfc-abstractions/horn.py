import z3

s = z3.SolverFor("HORN")
x, y, z = z3.Ints('x y z')
i, j, k = z3.Ints('i j k')
a, b = z3.Array('a', z3.IntSort(), z3.IntSort()), z3.Array('b', z3.IntSort(), z3.IntSort())
vars = [x, y, z, a, b]

s.add(z3.ForAll([a, i, j, k, x, y, z], z3.Implies(
    z3.And(
        a[i] == x,
        a[j] == y,
        a[k] == z,
        z3.ForAll([i], z3.Implies(z3.And(0 <= i, i < 10), a[i] != a[i + 1]))
    ),
    False
)))
# s.add(z3.ForAll(vars, z3.Implies(
#     z3.And(z3.Exists([i], a[i] == v) for v in [x, y, z]),
#     z3.Not(z3.ForAll([j], a[i] != a[j]))
# )))
# s.add(z3.ForAll([i], a[i] != a[i + 1]))
s.set("timeout", 1000)
r = s.check()
if r == z3.sat:
    print(s.model())
elif r == z3.unsat:
    print("unsat")
    # Generate a counterexample
    s = z3.SolverFor("HORN")
    s.add(z3.ForAll([a, i, j, k, x, y, z], z3.Implies(
        z3.And(
            a[i] == x,
            a[j] == y,
            a[k] == z,
            z3.ForAll([i], z3.Implies(z3.And(0 <= i, i < 10), a[i] != a[i + 1]))
        ),
        False
    )))
    r = s.check()
    if r == z3.sat:
        print("counterexample:")
        print(s.model())
    elif r == z3.unsat:
        print("no counterexample")
    else:
        print("unknown")
        assert r == z3.unknown
        print(s.reason_unknown())
else:
    assert r == z3.unknown
    print(s.reason_unknown())
