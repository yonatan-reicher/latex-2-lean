from egglog import *
from analysis.ast import AstId

@function
def request_vec_get(v: Vec[AstId]) -> Unit: ... # type: ignore

@function
def request_vec_get_from(v: Vec[AstId], i: i64Like) -> Unit: ... # type: ignore

@function
def vec_get(v: Vec[AstId], idx: i64Like) -> AstId: ... # type: ignore

def _rule(i: i64, id: AstId, v: Vec[AstId]):
    yield rule(request_vec_get(v)).then(request_vec_get_from(v, 0))
    yield rule(request_vec_get_from(v, i), i < v.length()).then(
        set_(vec_get(v, i)).to(v[i]),
        request_vec_get_from(v, i + 1)
    )

rules = [
    _rule
]
