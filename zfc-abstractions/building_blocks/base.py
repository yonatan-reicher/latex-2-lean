from z3 import BoolRef, ModelRef

class EvalAt:
    def eval(self, m: ModelRef): return m.eval(self)
    def __matmul__(self, m: ModelRef): return self.eval(m)

class IGuarded:
    def guards(self) -> list[BoolRef]: return []

