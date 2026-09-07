from analysis.ast import Ast, AstId, AstKind, ast_rules, N_AST_KINDS
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

# ------ Parent Of -------------------------------------------------------------

def parent_of(root: Ast, a: Ast, b: Ast, elements: Vec[AstId]):
    rules = [
        # Var
        rule().then(),
        # Num
        rule().then(),
        # Add
        # root == add(a, b)  ==>  root parent of a, b
        rule(eq(root.kind()).to(AstKind.add(a.id(), b.id()))) \
            .then(root.parent_of(a), root.parent_of(b)),
        # Sub
        rule(eq(root.kind()).to(AstKind.sub(a.id(), b.id()))) \
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
        # Eq
        rule(eq(root.kind()).to(AstKind.eq(a.id(), b.id()))) \
            .then(root.parent_of(a), root.parent_of(b)),
    ]
    n_expected_rules = N_AST_KINDS + 1 # set comprehension has two
    assert len(rules) == n_expected_rules, f"{n_expected_rules - len(rules)} rules missing here!"
    return rules

# ------ Scope -----------------------------------------------------------------

def scope(a: Ast, b: Ast, s: Set[String], kind: AstKind, id: AstId):
    return [
        # All scopes are, at least, empty
        rule(eq(a).to(Ast(id, kind))).then(set_(a.scope()).to(set(['a']))),
        # Child scope
        rule(a.parent_of(b), eq(s).to(a.scope())).then(set_(b.scope()).to(s)),
    ]

# ------ All -------------------------------------------------------------------

all = [
    *ast_rules,
    is_set_of_is_finite_set,
    is_finite,
    parent_of,
    scope,
]
