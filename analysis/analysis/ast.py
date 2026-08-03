"""
An egglog representation of an AST of the language this static analysis runs on.

Provides the `Ast` class, along with `ast_rules` that needs to be added to the
e-graph when using.
"""

from analysis.utils import sorry
from egglog import *
from typing import Callable, TypeVar, Concatenate

# Disabling a false-negative type-checking error.
# mypy: disable-error-code="empty-body"
# This file uses `egglog`'s `Expr` class, which needs empty bodies in methods.

AstId = i64
AstIdLike = i64Like

N_AST_KINDS = 0

def _ast_kind_ctor[**A, B, Self](
        f: Callable[Concatenate[Self, A], B]
        ) -> Callable[A, B]:
    global N_AST_KINDS
    N_AST_KINDS += 1
    return classmethod(f) #type: ignore

class AstKind(Expr):
    """
    An algebraic data-type that holds the type of an AST node, and it's
    arguments.
    """
    @_ast_kind_ctor
    def var(cls, name: StringLike) -> AstKind: ...
    @_ast_kind_ctor
    def num(cls, n: i64Like) -> AstKind: ...
    @_ast_kind_ctor
    def add(cls, this: AstIdLike, other: AstIdLike) -> AstKind: ...
    @_ast_kind_ctor
    def sub(cls, this: AstIdLike, other: AstIdLike) -> AstKind: ...
    @_ast_kind_ctor
    def set(cls, elements: Vec[AstId]) -> AstKind: ...
    @_ast_kind_ctor
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
        ...

def set_ast_id_and_kind(
    id: AstId,
    kind: AstKind,
):
    # yield rule(Ast(id, kind)).then(union(Ast(id, kind).id()).with_(id))
    yield rule(Ast(id, kind)).then(set_(Ast(id, kind).id()).to(id))
    yield rule(Ast(id, kind)).then(union(Ast(id, kind).kind()).with_(kind))

ast_rules = [
    set_ast_id_and_kind,
]
