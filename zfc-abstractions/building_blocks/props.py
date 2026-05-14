import z3
from z3 import BoolSort, SortRef, ExprRef
from collections.abc import Callable

from .base import IGuarded
from .set import Element, Elements
from .mapping import MappingRef, promote
from .formula_manip import alpha_renaming


def ForAll(bound_vars, body):
    props = [g for v in bound_vars
                 for g in (v.guards() if isinstance(v, IGuarded) else [])]
    return z3.ForAll(bound_vars, props >> BoolSort().cast(body))

def Exists(bound_vars, body):
    props = [g for v in bound_vars
                 for g in (v.guards() if isinstance(v, IGuarded) else [])]
    return z3.Exists(bound_vars, props & BoolSort().cast(body))

fresh_idx = iter(range(0xffff))


def Sig(sort: SortRef | [SortRef], fbody: Callable[..., ExprRef], mnemonic='μ'):
    if not isinstance(sort, list): sort = [sort]
    vz = [promote(f'tmp${fresh_idx.__next__()}', s) for s in sort]
    vs = [promote(mnemonic, s) for s in sort]
    # this is a nasty trick to get variables with the same mnemonic.
    # these then go through alpha renaming in order to make sense
    return alpha_renaming(z3.Exists(vs, Exists(vz, fbody(*vz)).body()))


def Injective(f: MappingRef):
    x, y = Elements("x y", f.domain())
    return ForAll([x, y], (f[x] == f[y]) >> (x == y))

def Surjective(f: MappingRef):
    x, y = Element("x", f.domain()), Element("y", f.range())
    return ForAll([y], Exists([x], f[x] == y))

def Isomorphism(f: MappingRef):
    return Injective(f) & Surjective(f)

def Refl(R: MappingRef):
    x = Element("x", R.domain())
    return ForAll([x], R[x][x])

def Trans(R: MappingRef):
    x, y, z = Elements("x y z", R.domain())
    return ForAll([x, y, z], (R[x][y] >> (R[y][z] >> R[x][z])))

def AntiSymm(R: MappingRef):
    x, y = Elements("x y", R.domain())
    return ForAll([x, y], R[x][y] >> (R[y][x] >> (x == y)))
    
def PartialOrder(R: MappingRef):
    return Refl(R) & Trans(R) & AntiSymm(R)

def LinearOrder(R: MappingRef):
    x, y = Elements("x y", R.domain())
    return PartialOrder(R) & ForAll([x, y], R[x][y] | R[y][x])