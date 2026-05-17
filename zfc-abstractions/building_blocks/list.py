from z3 import SortRef, ArrayRef, ArithRef, ExprRef, Int, Array, IntSort, ModelRef, BoolRef, And, Implies, BoolSort, Lambda, If
from .base import EvalAt, IGuarded
from building_blocks.mapping import Mapping, MappingSort
from building_blocks.set import SetRef, Set
from .props import ForAll, Exists, Injective, fresh_idx
from dataclasses import dataclass
from typing import Any

class IndexSortRef(SetRef):
    _len: ArithRef

    def __repr__(self): return f"IndexSort({self.univ().__repr__()})"
    def _repr_html_(self): return f"IndexSort({self.univ()._repr_html_()})"

    def eval(self, m: ModelRef):
        l = m.eval(self._len, model_completion=True).py_value()
        assert isinstance(l, int)
        assert l >= 0, f"index for {self} was {l}"
        return list(range(l))

    def cast(self, i):
        return IntSort().cast(i)

    @classmethod
    def of(cls, len: ArithRef, arr: ArrayRef):
        s = IntSort()
        c = super().of(s, arr)
        c._len = len
        return c


def IndexSort(len: ArithRef):
    i = Int('i')
    return IndexSortRef.of(len, Lambda([i], And(0 <= i, i < len)))

class IndexRef(ArithRef, IGuarded):
    _len: ArithRef

    def guards(self) -> list[BoolRef]:
        return [(0 <= self) & (self < self._len)]

    @classmethod
    def of(cls, i: ArithRef, len: ArithRef):
        c = cls(i.ast, ctx=i.ctx)
        c._len = len
        return c

def Index(name: str, len: ArithRef):
    return IndexRef.of(Int(name), len)

@dataclass(frozen=True)
class ListRef(EvalAt, IGuarded):
    arr: ArrayRef
    len: ArithRef

    # `len()` and `_ in _` check that the return type is int/bool
    # def __len__(self): return self._len
    # def __contains__(self, e): ...
    def contains(self, e: ExprRef):
        i = Int('i')
        return Exists([i], (0 <= i) & (i < self.len) & (self.arr[i] == e))

    def __repr__(self): return f"List({self.arr}, {self.len})"

    def __getitem__(self, i: int):
        return self.arr[i]

    def guards(self) -> list[BoolRef]:
        return [0 <= self.len]

    def eval_len(self, m: ModelRef) -> int:
        l = m.eval(self.len, model_completion=True).py_value()
        assert isinstance(l, int)
        assert l >= 0, f"index for {self} was {l}"
        return l

    @property
    def dom(self):
        return IndexSort(self.len)

    def eval(self, m: ModelRef) -> list[Any]:
        l = self.eval_len(m)
        return [
            x @ m if isinstance(x := self.arr[i], EvalAt)
            else m.eval(self.arr[i], model_completion=True)
            for i in range(l)
        ]

def List(name: str, element_sort: SortRef):
    arr = Array(f"{name}.arr", IntSort(), element_sort)
    len = Int(f"{name}.len")
    return ListRef(arr, len)

def Nth(x: ExprRef, i: int | ArithRef, l: ListRef) -> BoolRef:
    return And(0 <= i, i < l.len, l.arr[i] == x)

def Sublist(l1: ListRef, l2: ListRef) -> BoolRef:
    i = Index('i', l1.len)
    f = Mapping('f', MappingSort(l1.dom, l2.dom))
    return Exists([f],
        ForAll([i], And(
            # The function is bounded by l₂'s range
            And(0 <= f[i], f[i] < l2.len),
            # The function sends indexes in l₁ to equal elements in l₂
            l1.arr[i] == l2.arr[f[i]],
            # The function is increasing
            Implies(i < l1.len - 1, f[i] < f[i + 1]),
        ))
    )

def ListConcat(l1: ListRef, l2: ListRef, l: ListRef) -> BoolRef:
    i = Index('i', l.len)
    return And(
        l1.len + l2.len == l.len,
        ForAll([i], l[i] == If(i < l1.len, l1[i], l2[i - l1.len]))
    )

def ListSingleton(x: ExprRef, l: ListRef) -> BoolRef:
    return And(l.len == 1, l[0] == x)

def ListSorted(l: ListRef) -> BoolRef:
    i = Index('i', l.len - 1)
    return ForAll([i], l[i] <= l[i + 1])
