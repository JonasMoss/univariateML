"""Map every strict flat fixture to named pieces and replay its old predicate.

Run from the repository root. The emitted table groups fixture labels by
recipe. The probe checks are finite cross-checks, not proofs of equality.
"""
import collections
import importlib.util
import itertools
import math
from pathlib import Path
import sys

sys.dont_write_bytecode = True
spec = importlib.util.spec_from_file_location("flat_maxima", Path("dev/validation/flat_maxima.py"))
flat = importlib.util.module_from_spec(spec)
spec.loader.exec_module(flat)

B = flat.COUNT_MAX
INF = math.inf


def interval(lo=-INF, hi=INF, lower=False, upper=False, integer=False):
    return lo, hi, lower, upper, integer


REAL = interval()
POS = interval(0)
NONNEG = interval(0, lower=True)
UNIT = interval(0, 1, True, True)
PROB = interval(0, 1, False, True)


def count(lo=0, hi=INF):
    return interval(lo, hi, True, math.isfinite(hi), True)


def box(**axes):
    return "box", axes, ()


def constrained(edges, **axes):
    return "box", axes, edges


def points(axis, *values, **fixed):
    return "points", [dict(fixed, **{axis: val}) for val in values]


def recipe(family, key):
    zero = key.startswith("zeros-")
    if "adjacent-" in key:
        axis, pair = {
            "sizes": ("size", (1, 2)), "compositions": ("m", (2, 3)),
            "draws": ("k", (2, 3)), "populations": ("N", (3, 4)),
            "stops": ("r", (1, 2)),
        }[key.rsplit("-", 1)[1]]
        return "adjacent pair", [points(axis, *pair)]
    if family == "nhyper":
        edges = (("m", "N"),) if zero else (("m", "N"), ("N", "m"))
        return ("zero urn" if zero else "deterministic urn",
                [constrained(edges, m=count(), N=count(0 if zero else 7, B), r=0 if zero else 7)])
    if family == "hyper":
        domain = dict(m=count(), N=count(0 if zero else 4, B), k=count())
        pieces = []
        for a, b in (("m", "k"), ("k", "m")):
            axes = dict(domain, **{a if zero else b: 0 if zero else 4})
            edges = [("m", "N"), ("k", "N")]
            if not zero:
                edges.append(("N", a))
            pieces.append(constrained(tuple(edges), **axes))
        return "two urn faces", pieces
    if family in ("binom", "nbinom", "cmb", "zib", "zinb"):
        nb = family in ("nbinom", "zinb")
        domain = dict(size=NONNEG if nb else count(0, 10_000_000 if family == "cmb" else INF),
                      prob=PROB if nb else UNIT)
        if family == "cmb":
            domain["nu"] = REAL
        if family in ("zib", "zinb"):
            domain["pi"] = UNIT
        if "bernoulli" in key:
            return "Bernoulli dispersion", [box(size=1, prob=.5, nu=REAL)]
        if zero:
            faces = [box(**dict(domain, size=0)), box(**dict(domain, prob=1 if nb else 0))]
            if "pi" in domain:
                faces.append(box(**dict(domain, pi=1)))
            return "zero atom faces", faces
        return "positive atom", [box(**dict(domain, size=3 if family == "cmb" else 5, prob=1))]
    if family == "gpd":
        threshold = box(mu=0, sigma=1, xi=interval(-1, lower=True))
        translation = box(mu=interval(-1, 0, True, True), sigma=1, xi=-1)
        if key.endswith("threshold-scale"):
            return "threshold line", [threshold]
        if key.endswith("uniform-translation"):
            return "translation interval", [translation]
        return "union of threshold and translation", [threshold, translation]
    if family == "tlambda":
        if key.endswith("support-contacts"):
            return "support points", [points("mu", -2, 2, sigma=3, **{"lambda": 1.5})]
        shape = 2 if key.endswith("uniform-2") else 1
        return "uniform translation", [box(mu=interval(-2/shape, 2/shape, True, True),
                                            sigma=2, **{"lambda": shape})]
    if family == "sged":
        root = flat._SGED_XI
        return "enclosed reciprocal points", [("enclosed", [dict(xi=x) for x in (root, 1/root)])]
    if family == "genbeta":
        return "product", [("product", "shape1", "shape3", 1/math.log(2), dict(shape2=1, scale=1))]
    if family == "triang":
        return "two modes", ([points("c", .25, .75, a=0, b=1)] if key.endswith("held-endpoints")
                             else [("points", [dict(a=0, b=1.5, c=0), dict(a=-.5, b=1, c=1)])])
    if family == "tbinom":
        x = 0 if zero else 3
        return "singleton window", [box(size=count(x), prob=interval(0, 1, zero, False)),
                                    box(size=x, prob=1)]
    if family in flat.HURDLES or family in ("ztbinom", "ztnbinom"):
        domains = dict(zmpois={"lambda": NONNEG}, zmgeom=dict(prob=PROB),
                       zmlogarithmic=dict(prob=interval(0, 1, True)),
                       zmbinom=dict(size=count(1), prob=UNIT),
                       zmnbinom=dict(size=NONNEG, prob=PROB))
        if zero:
            p0 = .5 if key.endswith("interior") else 1
            return "absent positive component", [box(p0=p0, **domains[family])]
        extra = dict(p0=0) if family.startswith("zm") else {}
        if family.endswith("nbinom"):
            return "one atom", [box(size=NONNEG, prob=1, **extra)]
        return "one atom faces", [box(size=1, prob=UNIT, **extra), box(size=count(1), prob=0, **extra)]
    simple = {
        "bbinom": box(size=0, alpha=POS, beta=POS),
        "cmp": box(nu=NONNEG, **{"lambda": 0}),
        "pig": box(mean=POS, shape=0),
        "dweibull": box(shape1=.5, shape2=POS),
        "skellam": box(mu1=0 if zero else 1, mu2=0),
        "gompertz": box(a=1, b=POS),
        "gev": box(mu=0, sigma=1, xi=interval(-1, 1, True, True)),
        "sn": box(xi=0, omega=1, alpha=REAL),
        "st": box(xi=0, omega=1, alpha=REAL, nu=1),
        "zip": box(pi=1, **{"lambda": POS}),
        "tpois": box(**{"lambda": NONNEG if zero else POS}),
        "zipf": box(s=REAL),
    }
    return "coordinate box", [simple[family]]


def in_axis(v, axis):
    if not isinstance(axis, tuple):
        return v == axis
    lo, hi, lc, hc, integer = axis
    return ((v > lo or lc and v == lo) and (v < hi or hc and v == hi)
            and (not integer or int(v) == v))


def member(piece, row):
    tag, *args = piece
    if tag == "box":
        axes, edges = args
        return (all(in_axis(row[k], axis) for k, axis in axes.items())
                and all(row[a] <= row[b] for a, b in edges))
    if tag in ("points", "enclosed"):
        return any(all((math.isclose(row[k], v, rel_tol=1e-9) if tag == "enclosed"
                        else row[k] == v) for k, v in point.items()) for point in args[0])
    a, b, target, fixed = args
    return (all(row[k] == v for k, v in fixed.items()) and row[a] > 0 and row[b] > 0
            and math.isclose(row[a]*row[b], target, rel_tol=1e-9))


def coordinates(piece):
    tag, *args = piece
    if tag == "box":
        return args[0].keys()
    if tag in ("points", "enclosed"):
        return args[0][0].keys()
    return (*args[-1], *args[:2])


def main():
    tests = 0
    for family, cases in flat.CASES.items():
        groups = collections.defaultdict(list)
        for key, case in cases.items():
            name, pieces = recipe(family, key)
            groups[name].append(key)
            design = {"tbinom": {"a", "b"}, "tpois": {"a", "b"}, "zipf": {"N"}}
            names = sorted(set(case.held).union(*(coordinates(p) for p in pieces))
                           - design.get(family, set()))
            grid = set((-2, -1, -.5, 0, .25, .5, .75, 1, 1.5, 2, 3, 4, 5, 7, 10, 12, 50,
                        B-1, B, B+1, 10_000_000, 10_000_001, flat._SGED_XI,
                        1/flat._SGED_XI, 1/math.log(2)))
            grid.update(case.held.values())
            rows = itertools.product(*( (case.held[k],) if k in case.held else grid for k in names))
            found = 0
            for values in rows:
                row = dict(zip(names, values))
                # The old predicates assume the family domain was checked.
                if family == "nhyper" and "r" in row and row["r"] > row["m"]:
                    continue
                expect = case.member(**row)
                got = any(member(piece, row) for piece in pieces)
                assert got == expect, (family, key, row, expect)
                found += got
                tests += 1
            assert (found == 1 if case.unique else found >= 2), (family, key, found)
        for name, keys in groups.items():
            print(f"{family:14} {name:34} {', '.join(keys)}")
    print(f"{sum(map(len, flat.CASES.values()))} fixtures, {tests} hold-sliced probes")


if __name__ == "__main__":
    main()
