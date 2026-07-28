"""
An egglog representation of an AST of the language this static analysis runs on.

Provides the `Ast` class, along with `ast_rules` that needs to be added to the
e-graph when using.
"""

from analysis.utils import sorry
from egglog import *

# Disabling a false-negative type-checking error.
# mypy: disable-error-code="empty-body"
# This file uses `egglog`'s `Expr` class, which needs empty bodies in methods.

AstId = i64
AstIdLike = i64Like

class AstKind(Expr):
    """
    An algebraic data-type that holds the type of an AST node, and it's
    arguments.
    """
    @classmethod
    def var(cls, name: StringLike) -> AstKind: ...
    @classmethod
    def num(cls, n: i64Like) -> AstKind: ...
    def __add__(self, other: AstKind) -> AstKind: ...
    def __sub__(self, other: AstKind) -> AstKind: ...
    @classmethod
    def set_expr(cls, elements: Vec[AstKind]) -> AstKind: ...
    @classmethod
    def error(cls, msg: StringLike) -> AstKind: ...

class Ast(Expr):
    """ An AST node that holds an id and some data. """
    def __init__(self, id: AstIdLike, kind: AstKind): ...
    # These functions have to be implemented on the e-graph.
    def id(self) -> AstId: ...
    def kind(self) -> AstKind: ...
    # These are properties that should be given by the static analysis when
    # appropriate.
    def is_set(self) -> Unit:
        """ Is this expression always evaluating to a set? """
        ...
    def is_finite(self) -> Unit:
        """ Is this expression always a *finite* set? """
        ...
    def used_as_finite(self) -> Unit:
        """ Is this used somewhere as if it were a finite set? """

def set_ast_id_and_kind(
    id: AstId,
    kind: AstKind,
):
    # yield rule(Ast(id, kind)).then(union(Ast(id, kind).id()).with_(id))
    yield rule(Ast(id, kind)).then(set_(Ast(id, kind).id()).to(id))
    yield rule(Ast(id, kind)).then(set_(Ast(id, kind).kind()).to(kind))

ast_rules = [
    set_ast_id_and_kind,
]
