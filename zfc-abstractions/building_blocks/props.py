import z3
from z3 import Const, Consts, BoolSort, BoolVal, SortRef, ExprRef, \
               And, Or, Not, If, Map, Distinct, K
from collections.abc import Callable

from .base import IGuarded
from .set import SetRef, Element, Elements
from .mapping import MappingRef, promote
from .formula_manip import alpha_renaming


def ForAll(bound_vars, body):
    props = IGuarded.guards(bound_vars)
    return z3.ForAll(bound_vars, props >> BoolSort().cast(body))

def Exists(bound_vars, body):
    props = IGuarded.guards(bound_vars)
    return z3.Exists(bound_vars, props & BoolSort().cast(body))

fresh_idx = iter(range(0xffff))


def Prod(sort: SortRef | [SortRef], fbody: Callable[..., ExprRef], mnemonic='μ'):
    return quantifier_core(sort, fbody, mnemonic, ForAll, z3.ForAll)

def Sig(sort: SortRef | [SortRef], fbody: Callable[..., ExprRef], mnemonic='μ'):
    return quantifier_core(sort, fbody, mnemonic, Exists, z3.Exists)

def quantifier_core(sort: SortRef | [SortRef], fbody: Callable[..., ExprRef], mnemonic,
                    quant, z3_quant):
    if not isinstance(sort, list): sort = [sort]
    vz = [promote(f'tmp${fresh_idx.__next__()}', s) for s in sort]
    vs = [promote(mnemonic, s) for s in sort]
    # this is a nasty trick to get variables with the same mnemonic.
    # these then go through alpha renaming in order to make sense
    return alpha_renaming(z3_quant(vs, quant(vz, fbody(*vz)).body()))

def popcnt(*bools: [BoolRef]):
    return Sum(*(If(v, 1, 0) for v in bools))

def one_of(*bools: [BoolRef]):
    '''valuates to `True` iff exactly one of `bools` is true.'''
    #return popcnt(*bools) == 1  # - this also works but is more prone to `unknown`s
    match len(bools):
        case 0: return BoolVal(False)
        case 1: return bools[0]
        case 2: return bools[0] ^ bools[1]
        case _:
            return If(bools[0], Not(Or(*bools[1:])), one_of(*bools[1:]))

##
# Set operations and properties
##

def subset(s1, s2):
    u1, u2 = s1.univ(), s2.univ()
    if u1 == u2:
        x, y = Consts('x y', u1)
        return ForAll([x, y], s1[x] >> s2[x])
    else:
        z = Const('z', u1)
        return ForAll([z], Not(s1[z]))

def disjoint(s1, s2):
    u1, u2 = s1.univ(), s2.univ()
    if u1 == u2:
        x = Const('x', u1)
        return ForAll([x], Not(s1.set[x] & s2.set[x]))
    else:
        return BoolVal(True)

def union(s1, s2):
    u1, u2 = s1.univ(), s2.univ()
    if u1 == u2:
        return SetRef.of(u1, Map(Or().decl(), s1.set, s2.set))
    else:
        raise TypeError('universe sort mismatch')

def intersection(s1, s2):
    u1, u2 = s1.univ(), s2.univ()
    if u1 == u2:
        return SetRef.of(u1, Map(And().decl(), s1.set, s2.set))
    else:
        return SetRef.of(u1, K(u1, False))

def set_diff(s1, s2):
    u1, u2 = s1.univ(), s2.univ()
    if u1 == u2:
        return SetRef.of(u1, Map(And().decl(), s1.set, Map(Not(False).decl(), s2.set)))
    else:
        return SetRef.of(u1, K(u1, False))

def card_geq(s: SetRef, n: int):
    return Sig([s] * n, Distinct)

def isomorphic(s1, s2):
    return Sig(s1 ** s2, lambda f: Isomorphism(f))

##
# Classes of functions
##

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