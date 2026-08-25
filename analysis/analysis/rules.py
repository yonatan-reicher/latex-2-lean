from analysis.ast import Ast, AstId, AstKind, ast_rules
from egglog import *

def is_set_of_is_finite_set(ast: Ast):
    yield rule(ast.is_finite()).then(ast.is_set())

def is_finite(ast: Ast, elements: Vec[i64], name: String, other: Ast, def_id: AstId, var_id: AstId):
    # Set
    yield rule(eq(ast.kind()).to(AstKind.set(elements))).then(ast.is_finite())
    # Var
    yield rule(
        eq(ast.kind()).to(AstKind.var(name)),
        AstKind.definition(Ast(def_id, AstKind.eq(Ast(var_id, AstKind.var(name)).id(), other.id())).id()),
        other.is_finite()
    ).then(
        ast.is_finite()
    )

all = [
    *ast_rules,
    is_set_of_is_finite_set,
    is_finite,
]
