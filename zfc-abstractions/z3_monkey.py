from z3 import *
from functools import reduce

SortRef.__pow__ = lambda dom, rng: mk_mapping(as_sort(dom), as_sort(rng))
SortRef.__rpow__ = lambda rng, dom: mk_mapping(as_sort(dom), as_sort(rng))
BoolRef.__bwor__ = Or
BoolRef.__bwxor__ = Xor

def _implies(lhs, rhs):
    if isinstance(lhs, list):
        return reduce(lambda r, l: l >> r, reversed(lhs), rhs)
    else:
        return Implies(lhs, rhs)

def _and(lhs, rhs):
    return And(*flatten(lhs), rhs)

def flatten(*n):
    return (e for a in n
        for e in (flatten(*a) if isinstance(a, (tuple, list)) else (a,)))

BoolRef.__and__ = _and
BoolRef.__rand__ = lambda y, x: _and(x, y)

BoolRef.__rshift__ = _implies
BoolRef.__rrshift__ = lambda y, x: _implies(x, y)

SortRef_cast = SortRef.cast
_Consts = Consts

mk_mapping = ArraySort

def as_sort(expr):
    if expr is bool: return BoolSort()
    return expr