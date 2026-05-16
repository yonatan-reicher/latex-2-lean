from functools import reduce

from z3 import *
from .base import EvalAt, IGuarded
from .set import Element, ElementRef, univ


class MappingSortRef(ArraySortRef):
    _dom: SortRef
    _rng: SortRef

    def domain(self): return self._dom
    def range(self): return self._rng
    
    @classmethod
    def of(cls, s: ArraySortRef, dom: SortRef, rng: SortRef):
        c = cls(s.ast, ctx=s.ctx)
        c._dom, c._rng = dom, rng
        return c

    def __repr__(self): return f"{self._dom.__repr__()} → {self._rng.__repr__()}"
    def _repr_html_(self): return f"{self._dom._repr_html_()} → {self._rng._repr_html_()}"

def MappingSort(dom: SortRef, rng: SortRef):
    return MappingSortRef.of(ArraySort(dom, rng), dom, rng)


class MappingRef(ArrayRef, EvalAt, IGuarded):
    _sort: SortRef

    def sort(self): return self._sort

    def guards(self):
        from .props import ForAll, Exists
        x, y = Element('x', self.domain()), Element('y', self.range())
        return [ForAll([x], Exists([y], self[x] == y))]

    @classmethod
    def of(cls, e: ArrayRef, sort: SortRef):
        e = cls(e.ast, ctx=e.ctx)
        e._sort = sort
        return e

    def eval(self, m: ModelRef):
        dom, rng = self.domain(), self.range()
        return {e: (promote(self[e], rng) @ m) for e in univ(dom, m)}

def Mapping(name: str, sort: MappingSortRef):
    return MappingRef.of(Const(name, sort), sort)


def promote(e: ExprRef | str, sort: SortRef):
    if isinstance(e, str): e = Const(e, sort)
    if isinstance(sort, MappingSortRef):
        return MappingRef.of(e, sort)
    else:
        return ElementRef.of(e, sort)

def array_from_pairs(kv_pairs: [(ExprRef, ExprRef)], bot: ExprRef):
    return reduce(lambda arr, kv: Store(arr, kv[0], kv[1]), kv_pairs,
                  K(bot.sort(), bot))

def mapping_from_pairs(sort: MappingSortRef,
                       kv_pairs: [(ExprRef, ExprRef)], bot: ExprRef=None):
    U = sort.domain().univ()
    if bot is None: bot = Element('⊥', U)
    return MappingRef.of(array_from_pairs(kv_pairs, bot), sort)


class TabularMappingRef(MappingRef):
    '''
    A special case of mapping in which entries are given as a dictionary.
    '''
    table: dict
    
    @classmethod
    def of(cls, e: ArrayRef, sort: SortRef=None, table: dict={}):
        if sort is None: sort = e.sort()
        e = super(TabularMappingRef, cls).of(e, sort)
        e.table = table
        return e

def TabularMapping(sort: MappingSortRef, table: dict):
    return TabularMappingRef.of(
        mapping_from_pairs(sort, table.items()), table=table)

def tabulate(f: MappingRef | str, sort: MappingSortRef=None, table=None):
    if sort is None:
        f, sort = f.decl().name(), f.sort()
    if table is None:
        U = sort.domain().univ()
        table = {u: Element(f"{f}‹{u}›", U) for u in univ(U, None)}
    return TabularMapping(sort, table=table)
