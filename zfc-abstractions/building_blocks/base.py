from z3 import AstRef, BoolRef, ModelRef

class EvalAt:
    def eval(self, m: ModelRef) -> Any: return m.eval(self, model_completion=True)
    def __matmul__(self, m: ModelRef): return self.eval(m)

class IGuarded:
    def guards(self) -> list[BoolRef]: return []

    @classmethod
    def guards(cls, decls: list[AstRef]):
        return [g for v in decls
                    for g in (v.guards() if isinstance(v, IGuarded) else [])]

