from z3 import *
from .base import EvalAt, IGuarded


class SetRef(SortRef, EvalAt):
    set: ArrayRef  # U -> bool

    def univ(self): return self.set.domain()

    def __getitem__(self, e: ExprRef): return self.set[e]
    def __contains__(self, e: ExprRef): return self.set[e]
    
    @classmethod
    def of(cls, s: SortRef, set: ArrayRef):
        c = cls(s.ast, ctx=s.ctx)
        c.set = set
        return c

    def __repr__(self): return self.set.__repr__()
    def _repr_html_(self): return self.set._repr_html_()

    def eval(self, m):
        return [ElementRef.of(e, self) for e in univ(self.set.domain(), m)
                if m.eval(self.set[e], model_completion=True)]

def Set(name: str, univ: SortRef):
    return SetRef.of(univ, Array(name, univ, BoolSort()))


class ElementRef(ExprRef, EvalAt, IGuarded):
    _sort: SortRef

    def sort(self): return self._sort

    def guards(self):
        return [self._sort.set[self]] if isinstance(self._sort, SetRef) else []
    
    @classmethod
    def of(cls, e: ExprRef, sort: SortRef):
        e = cls(e.ast, ctx=e.ctx)
        e._sort = sort
        return e

def Element(name: str, sort: SortRef):
    return ElementRef.of(Const(name, sort), sort)

def Elements(names: str, sort: SortRef):
    return tuple(Element(name, sort) for name in names.split())


def univ(s: SortRef, m: ModelRef):
    if isinstance(s, DatatypeSortRef):
        return [s.constructor(i)() for i in range(s.num_constructors())]
    elif isinstance(s, SetRef):
        return s.eval(m)
    else:
        return m.get_universe(s)