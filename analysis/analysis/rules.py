from analysis.ast import Ast, AstKind, ast_rules
from egglog import *

def is_set_of_is_finite_set(ast: Ast):
    yield rule(ast.is_finite()).then(ast.is_set())

def is_finite(ast: Ast, elements: Vec[i64]):
    yield rule(eq(ast.kind()).to(AstKind.set(elements))).then(
        ast.is_finite())

all = [
    *ast_rules,
    is_set_of_is_finite_set,
    is_finite,
]
