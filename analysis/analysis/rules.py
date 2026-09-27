from analysis.ast import Ast, AstId, AstKind, ast_rules, N_AST_KINDS
from analysis.bin_op import *
from analysis.vec_get import vec_get, request_vec_get, rules as vec_get_rules
from egglog import *
from typing import Any, Callable

# ------ Helpers ---------------------------------------------------------------

def variable_rules(f: Callable[[Ast], Fact | BaseExpr]):
    def func(
        var_1: Ast,
        var_2: Ast,
        name: String,
        definition: Ast,
        x: Ast,
    ):
        return [
            # Var ⇒ Def
            rule(
                Ast(var_1.id(), AstKind.var(name)),
                AstKind.definition(definition.id()),
                Ast(definition.id(), AstKind.bin_op(EQ, var_2.id(), x.id())),
                Ast(var_2.id(), AstKind.var(name)),
                f(var_2),
            ).then(f(x)),
            # Def ⇒ Var
            rule(
                Ast(var_1.id(), AstKind.var(name)),
                AstKind.definition(definition.id()),
                Ast(definition.id(), AstKind.bin_op(EQ, var_2.id(), x.id())),
                Ast(var_2.id(), AstKind.var(name)),
                f(x),
            ).then(f(var_2)),
        ]
    return func

# ------ Is Finite -------------------------------------------------------------

def is_set_of_is_finite_set(ast: Ast):
    yield rule(ast.is_finite()).then(ast.is_set())

def is_finite(
    ast: Ast,
    elements: Vec[i64],
):
    # Set
    yield rule(eq(ast.kind()).to(AstKind.set(elements))).then(ast.is_finite())

# ------ Used As Finite --------------------------------------------------------

def used_as_finite(
    # App
    app: Ast,
    arg: Ast,
):
    return [
        # Abs
        rule(Ast(app.id(), AstKind.app("\\abs", [arg.id()])))
            .then(app.used_as_finite()),
    ]

# ------ Parent Of -------------------------------------------------------------

def parent_of(
    root: Ast,
    a: Ast,
    b: Ast,
    elements: Vec[AstId],
    bin_op: BinOp,
    # app
    f: String,
    i: i64,
    args: Vec[AstId],
    arg: Ast,
):
    rules = [
        # Var
        rule().then(),
        # Num
        rule().then(),
        # Binary operator
        # root == a op b  ==>  root parent of a, b
        rule(eq(root.kind()).to(AstKind.bin_op(bin_op, a.id(), b.id()))) \
            .then(root.parent_of(a), root.parent_of(b)),
        # Set
        rule(eq(root.kind()).to(AstKind.set(elements)), elements.contains(a.id())) \
            .then(root.parent_of(a)),
        # Set Comprehension
        rule(eq(root.kind()).to(AstKind.setComp(a.id(), elements))) \
            .then(root.parent_of(a)),
        rule(eq(root.kind()).to(AstKind.setComp(b.id(), elements)), elements.contains(a.id())) \
            .then(root.parent_of(a)),
        # Definition
        rule(eq(root.kind()).to(AstKind.definition(a.id()))).then(root.parent_of(a)),
        # Axiom
        rule(eq(root.kind()).to(AstKind.axiom(a.id()))).then(root.parent_of(a)),
        # Application
        rule(
            eq(root.kind()).to(AstKind.app(f, args)),
            eq(arg.id()).to(vec_get(args, i)),
        ).then(root.parent_of(arg)),
        rule(eq(root.kind()).to(AstKind.app(f, args)))
            .then(request_vec_get(args)),
        # Error
        rule().then(),
    ]
    n_expected_rules = N_AST_KINDS + 2 # set comprehension and application both have two rules
    assert len(rules) == n_expected_rules, f"{n_expected_rules - len(rules)} rules missing here!"
    return rules

# ------ Scope -----------------------------------------------------------------

def scope(
    a: Ast, b: Ast, s: Set[String], kind: AstKind, id: AstId,
    # Set comprehension
    lhs: Ast,
    var_id: AstId,
    var_name: String,
    bound_id: AstId,
    binders: Vec[AstId],
    binder_id: AstId,
    binder1_id: AstId,
    binder2_id: AstId,
    binder1_index: AstId,
    binder2_index: AstId,
    binder2: Ast,
):
    return [
        # All scopes are, at least, empty
        rule(eq(a).to(Ast(id, kind))).then(set_(a.scope()).to(set())),
        # Child scope
        rule(a.parent_of(b), eq(s).to(a.scope())).then(set_(b.scope()).to(s)),
        # Set comprehension
        rule( # lhs
            AstKind.setComp(lhs.id(), binders),
            binders.contains(Ast(
                binder_id,
                AstKind.bin_op(
                    IN_,
                    Ast(var_id, AstKind.var(var_name)).id(),
                    bound_id,
                ),
            ).id()),
        ).then(
            set_(lhs.scope()).to(set([var_name])),
        ),
        rule( # right
            AstKind.setComp(lhs.id(), binders),
            eq(binder1_id).to(vec_get(binders, binder1_index)),
            eq(binder2_id).to(vec_get(binders, binder2_index)),
            binder1_index < binder2_index,
            Ast(
                binder1_id,
                AstKind.bin_op(
                    IN_,
                    Ast(var_id, AstKind.var(var_name)).id(),
                    bound_id,
                ),
            ),
            eq(binder2_id).to(binder2.id()),
        ).then(
            set_(binder2.scope()).to(set([var_name])),
        ),
        rule(AstKind.setComp(lhs.id(), binders)).then(request_vec_get(binders)),
    ]

# ------ All -------------------------------------------------------------------

all = [
    # Import rules
    *ast_rules,
    *vec_get_rules,
    # Is Finite
    is_set_of_is_finite_set,
    is_finite,
    variable_rules(Ast.is_finite),
    # Used As Finite
    used_as_finite,
    variable_rules(Ast.used_as_finite),
    # Other
    parent_of,
    scope,
]
