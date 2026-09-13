from analysis.ast import Ast, AstId, AstKind, ast_rules, N_AST_KINDS
from analysis.bin_op import *
from egglog import *

def is_set_of_is_finite_set(ast: Ast):
    yield rule(ast.is_finite()).then(ast.is_set())

def is_finite(ast: Ast, elements: Vec[i64], name: String, other: Ast, def_id: AstId, var_id: AstId):
    # Set
    yield rule(eq(ast.kind()).to(AstKind.set(elements))).then(ast.is_finite())
    # Var
    yield rule(
        eq(ast).to(Ast(var_id, AstKind.var(name))),
        AstKind.definition(Ast(def_id, AstKind.bin_op(EQ, var_id, other.id())).id()),
        other.is_finite()
    ).then(
        ast.is_finite()
    )

# ------ Parent Of -------------------------------------------------------------

def parent_of(root: Ast, a: Ast, b: Ast, elements: Vec[AstId], bin_op: BinOp):
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
        # Error
        rule().then(),
    ]
    n_expected_rules = N_AST_KINDS + 1 # set comprehension has two
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
):
    return [
        # All scopes are, at least, empty
        rule(eq(a).to(Ast(id, kind))).then(set_(a.scope()).to(set())),
        # Child scope
        rule(a.parent_of(b), eq(s).to(a.scope())).then(set_(b.scope()).to(s)),
        # Set comprehension
        rule(
            AstKind.setComp(lhs.id(), binders),
            binders.contains(Ast(
                binder_id,
                AstKind.bin_op(
                    IN,
                    Ast(var_id, AstKind.var(var_name)).id(),
                    bound_id,
                ),
            ).id()),
        ).then(
            set_(lhs.scope()).to(set([var_name])),
        ),
        rule(
            AstKind.setComp(lhs.id(), binders),
            binders.contains(Ast(
                binder_id,
                AstKind.bin_op(
                    IN,
                    Ast(var_id, AstKind.var(var_name)).id(),
                    bound_id,
                ),
            ).id()),
        ).then(
            set_(lhs.scope()).to(set([var_name])),
        ),
    ]

# ------ All -------------------------------------------------------------------

all = [
    *ast_rules,
    is_set_of_is_finite_set,
    is_finite,
    parent_of,
    scope,
]
