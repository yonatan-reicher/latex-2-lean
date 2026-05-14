from z3 import *


def alpha_renaming(phi):
    return traverse(phi, RenamingVisitor())


class Visitor:
    def visit_quantifier(self, e):
        bound_vars = self.visit_bound_vars(self.bound_vars(e))
        body = traverse(e.body(), self)
        if e.is_forall():
            return ForAll(bound_vars, body)
        else:
            return Exists(bound_vars, body)        

    def visit_decl(self, e):
        return e.decl()([traverse(c, self) for c in e.children()])

    def visit_leaf(self, e):
        return e

    def visit_bound_vars(self, bound_vars):
        return bound_vars

    def bound_vars(self, e):
        return [Const(e.var_name(i), e.var_sort(i))
                for i in range(e.num_vars())]


class RenamingVisitor(Visitor):
    def __init__(self):
        self.names = set()
        self.bound_vars_renamed = {}

    def visit_quantifier(self, e):
        return super(RenamingVisitor, self).visit_quantifier(e)

    def visit_bound_vars(self, bound_vars):
        renamed = []
        for v in bound_vars:
            name = v.decl().name()
            if name in self.names:
                name = self.fresh(strip_subscript(name))
                v = Const(name, v.sort())
            self.names.add(name)
            renamed += [v]
        return renamed

    def fresh(self, prefix):
        for i in range(0xffff):
            name = f'{prefix}{subscript(i)}'
            if name not in self.names:
                return name
        raise AssertionError()


def traverse(e, visitor=None):
    if is_quantifier(e):
        return visitor.visit_quantifier(e)
    elif e.children():
        return visitor.visit_decl(e)
    else:
        return visitor.visit_leaf(e)


def subscript(n):
    '''generates a subscript numeral because subscript numerals are pretty neat'''
    if n == 0: return '₀'
    s = ''
    while n > 0:
        s = '₀₁₂₃₄₅₆₇₈₉'[n % 10] + s
        n = n // 10
    return s

def strip_subscript(s):
    while s and s[-1] in '₀₁₂₃₄₅₆₇₈₉':
        s = s[:-1]
    return s or 'ε'