from z3 import AstRef, BoolRef, ModelRef

class EvalAt:
    # TODO: Shouldn't this function be top level, and check via isinstance what
    # to do? Or instead, we could monkey-patch the object class.
    def eval(self, m: ModelRef) -> Any: return m.eval(self, model_completion=True)
    def __matmul__(self, m: ModelRef): return self.eval(m)

class IGuarded:
    # TODO: See above todo.
    def guards(self) -> list[BoolRef]: return []

    @classmethod
    def guards(cls, decls: list[AstRef]):
        return [g for v in decls
                    for g in (v.guards() if isinstance(v, IGuarded) else [])]

