from z3 import SortRef, ExprRef, EnumSort, DatatypeSortRef

class FinSortRef(SortRef):
    elements: list[ExprRef]

    def values(self): return self.elements

    # walks like a list of elements
    def __getitem__(self, i: int): return self.elements[i]
    def __len__(self): return len(self.elements)
    def __iter__(self): return iter(self.elements)
    
    @classmethod
    def of(cls, e: tuple[DatatypeSortRef, list[ExprRef]]):
        c = cls(e[0].ast, ctx=e[0].ctx)
        c.elements = e[1]
        return c

def FinSort(name: str, element_names: list[str]):
    return FinSortRef.of(EnumSort(name, element_names))

Fin = {
    3: FinSort('○', ['⓪', '①', '②'])
}
