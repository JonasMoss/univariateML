"""Local branch comparisons on exact binary64 inputs, plus policy fixtures."""
from fractions import Fraction
import math

import mpmath as mp

M = mp.mpf


def avg(values, counts):
    return mp.fsum(v * n for v, n in zip(values, counts)) / sum(counts)


def exact(value):
    n, d = value.as_integer_ratio()
    return M(n) / d


def neighbors(value):
    rounded = float(value)
    upward = math.inf if exact(rounded) < value else -math.inf
    pair = sorted((rounded, math.nextafter(rounded, upward)))
    assert exact(pair[0]) < value < exact(pair[1])
    return pair


def roots(equations, seed):
    seed = tuple(M(x) for x in seed)
    answer = tuple(mp.findroot(equations, seed, tol=mp.eps * 100))
    assert max(map(abs, equations(*answer))) < M('1e-60')
    return answer


def softplus(z):
    # Omitting exp(-1000) and smaller terms is harmless at both replay precisions.
    return max(z, 0) if abs(z) > 1000 else max(z, 0) + mp.log1p(mp.exp(-abs(z)))


def logistic(z):
    if abs(z) > 1000:
        return M(int(z > 0))
    return 1/(1+mp.exp(-z))


def lomax(x, counts, t):
    z = [t + mp.log(v) for v in x]
    a = avg([softplus(v) for v in z], counts)
    q = avg([1 / (1 + mp.exp(v)) for v in z], counts)
    return (a * q - (1 - q)) / a, t - mp.log(a) - a


def two_lomax(make, counts, fixed=()):
    def calculate(s, t, r):
        x = make(r) if not fixed else [*fixed[:2], r, fixed[2]]
        ls, lo = lomax(x, counts, s)
        hs, hi = lomax(x, counts, t)
        return ls, hs, hi - lo

    def compare(r, s, t, fixed):
        x = make(r) if not fixed else [*fixed[:2], r, fixed[2]]
        s, t = roots(lambda s, t: (lomax(x, counts, s)[0],
                                    lomax(x, counts, t)[0]), (s, t))
        for peak in (s, t):
            assert mp.diff(lambda u: lomax(x, counts, u)[0], peak) < 0
        return lomax(x, counts, t)[1] - lomax(x, counts, s)[1]

    return calculate, compare


def frechet_free(x, counts, b, y):
    z = [g * mp.exp(-y) for g in x]
    t = [mp.log1p(b * g) / b for g in z] if b else z
    r = [g / (1 + b * g) for g in z]
    db = [(q - v) / b for q, v in zip(r, t)] if b else [-g*g/2 for g in z]
    w = [n * mp.exp(-v) for n, v in zip(counts, t)]
    value = -y - (1 + b) * avg(t, counts) - mp.log(sum(w)/sum(counts)) - 1
    sy = -1 + (1 + b) * avg(r, counts) - avg(r, w)
    sb = -avg(t, counts) - (1 + b) * avg(db, counts) + avg(db, w)
    return value, sy, sb


def frechet_held(x, counts, a, u):
    e = mp.exp(u/a)
    z = [mp.log(g + e) for g in x]
    q = [e / (g + e) for g in x]
    h = [mp.exp(-a*v) for v in z]
    su = avg([f * (v - 1 - 1/a) for f, v in zip(q, h)], counts)
    sa = 1/a + avg([(v-1)*j for v, j in zip(h, z)], counts) - u*su/a
    value = mp.log(a) - avg([(1+a)*j+v for j, v in zip(z, h)], counts)
    return value, su, sa


def frechet_route(kind):
    if kind == "held":
        profile, counts, low, edge = frechet_held, (3, 4, 1), None, M(1)
        seed = ('-.55', '-.75', '1.56', '39.45')
        levels = lambda r, fixed: [0, fixed[0] if fixed else M(1)/20, r]
        shape_direction = -1
    else:
        profile, edge, shape_direction = frechet_free, M(1), 1
        levels = lambda r, fixed: [0, 1, r]
        if kind == "face":
            counts, low = (4, 5, 1), M(0)
            seed = ('-.4', '-2.4', '1.46')
        else:
            counts, low = (1, 1, 1), None
            seed = ('-.28', '-1.56', '.12', '2.65')

    def scores(r, fixed, y0, y1, b=None, crossing=False):
        x = levels(r, fixed)
        b = low if b is None else b
        lval, lscore, lshape = profile(x, counts, b, y0)
        rval, rscore, rshape = profile(x, counts, edge, y1)
        if not crossing and low is not None:
            assert lshape < 0
        if not crossing:
            assert shape_direction * rshape > 0
        residuals = (lscore, rscore) if low is not None else (lscore, rscore, lshape)
        return (*residuals, rval-lval)

    def equality(*args):
        return scores(args[-1], (), *args[:-1], crossing=True)

    initial = roots(equality, seed)

    def compare(r, fixed):
        fit = roots(lambda *args: scores(r, fixed, *args, crossing=True)[:-1], initial[:-1])
        return scores(r, fixed, *fit)[-1]

    return initial[-1], compare, ([M(1)/20] if kind == "held" else [])


def folded(a, c):
    m2 = (99 + c*c) / 100
    v = 2*m2 / (1 + mp.sqrt(1+4*m2*a*a))
    mu = a*v
    value = (mp.log(m2/v)+1-(m2+mu*mu)/v)/2
    value += (99*mp.log(mp.cosh(a))+mp.log(mp.cosh(a*c)))/100
    return (99*mp.tanh(a)+c*mp.tanh(a*c))/100-mu, value


def gpd(b, a):
    xs = [a, a, M(1)]
    d = mp.fsum(mp.log1p(b*x) for x in xs)/(3*b)
    r = mp.fsum(x/(1+b*x) for x in xs)/3
    k = b*d
    return (1+k)*r-d, -mp.log(d)-k-1


def para(a, x):
    z = [-a*v for v in x]
    return (2/a + avg([-v-softplus(t)+(1+a)*v/(1+mp.exp(-t))
                       for v, t in zip(x, z)], (1, 1)),
            2*mp.log(a)+avg([t-(1+a)*softplus(t) for t in z], (1, 1)))


def burr(k, c, b, y, counts):
    logits = [c*v+b for v in y]
    probabilities = list(map(logistic, logits))
    h = list(map(softplus, logits))
    rate = [1-(1+k)*p for p in probabilities]
    value = mp.log(k*c)+avg([z-(1+k)*v for z,v in zip(logits,h)], counts)
    return value, (1-k*avg(h,counts),
                   1+c*avg([v*u for v,u in zip(y,rate)],counts),
                   avg(rate,counts))


def burr_routes():
    def profile(theta, y, counts):
        logk, logc, b = theta
        return burr(mp.exp(logk), mp.exp(logc), b, y, counts)

    def finite_face(x):
        y, counts = list(map(mp.log, x)), (1,)*7
        seed = ('.125','1.612','-3.009')
        fit = roots(lambda *args: profile(args,y,counts)[1], seed)
        def weibull(c):
            weights = [mp.exp(c*v) for v in y]
            return 1/c+avg(y,counts)-avg(y,weights)
        (c,) = roots(lambda c: (weibull(c),), ('5',))
        w = mp.log(c)+c*avg(y,counts)-mp.log(avg([mp.exp(c*v) for v in y],counts))-1
        return profile(fit,y,counts)[0]-w

    def finite_pair(x):
        y, counts = list(map(mp.log,x)), (2**90,)*7+(1,)
        seeds = (('-33.58','34.2','31.635'), ('-.213','1.68','-2.49'))
        fits = [roots(lambda *args: profile(args,y,counts)[1], seed) for seed in seeds]
        return profile(fits[1],y,counts)[0]-profile(fits[0],y,counts)[0]

    q = M('.99291020580472189458')
    t = M('.96024095654752758625')
    for kind, fun, y, index in [
        ("Burr joint finite/finite", finite_pair,
         [0,M('.23826')*q,7*q/15,8*q/15,8*q/15,1,1,-1], 2),
        ("Burr joint finite/Weibull", finite_face,
         [0,1-10*t/15,1-8*t/15,1-7*t/15,1-7*t/15,1,1], 1),
    ]:
        print('Solving', kind, 'at', mp.mp.dps, 'digits', flush=True)
        x = list(map(mp.exp,y))
        fixed = x[:index]+x[index+1:]
        # Keep duplicated levels duplicated. The perturbed index is a singleton.
        assert y.count(y[index]) == 1
        def compare(varied, fixed):
            return fun([*fixed[:index], varied, *fixed[index:]])
        dyadic = tuple(exact(float(v)) for v in fixed)
        (crossing,) = roots(lambda v: (compare(v,dyadic),), (x[index],))
        yield report_pair(kind, crossing, compare, fixed)


def survivor_ids(regions):
    lower = max(lo for lo, _ in regions.values())
    return {key for key, (_, hi) in regions.items() if hi >= lower}


def fixtures():
    assert survivor_ids({"finite": (1, 2), "limit": (0, 1)}) == {"finite", "limit"}
    assert survivor_ids({"finite": (1, 2), "limit": (0, Fraction(9, 10))}) == {"finite"}
    assert survivor_ids({"finite": (1, 2), "limit": (3, 4)}) == {"limit"}
    assert survivor_ids({"finite": (1, 1), "limit": (1, 1)}) == {"finite", "limit"}

    # Exact input predicates and ordinary likelihoods on the structural examples.
    f = Fraction
    assert (sorted([0, 2])[0], sorted([0, 2])[1], f(2, 2)) == (0, 2, 1)
    d, sigma = f(5), f(3)
    assert d*d-sigma*sigma == 4**2
    assert f((4+5)**2+9) * ((4-5)**2+9) == 4*d*d*sigma*sigma
    assert f(3) >= max(2, 0)-min(2, 0)
    for n in (2, 3, 17, 2**53+1):
        p = f(1, n)
        odds = (n-1)*p/(2*(1-p))
        assert odds == f(1, 2) and 1/(1+odds)**2*odds/(1+odds) == f(4, 27)


def report_pair(label, crossing, compare, fixed=()):
    fixed = tuple(float(x) for x in fixed)
    rows = {}
    for varied in neighbors(crossing):
        gap = compare(exact(varied), tuple(map(exact, fixed)))
        assert 0 < abs(gap) < M('1e-10')
        rows[varied.hex()] = gap
    return label, fixed, rows


def run():
    rows = []
    calculate, compare = two_lomax(lambda a: [a, 1, 100], (1,10,10))
    s, t, u = roots(lambda s,t,u: calculate(s,t,mp.exp(u)), ('.1','60.5','-59.86'))
    rows.append(report_pair("Lomax", mp.exp(u), lambda a,f: compare(a,s,t,f)))

    calculate, compare = two_lomax(None, (1,1,1,1),
                                   (M(1), mp.exp(M('5.1')), mp.exp(8)))
    s, t, u = roots(lambda s,t,u: calculate(s,t,mp.exp(u)), ('-4.53','-1.35','6.009'))
    rows.append(report_pair("Burr held power 1", mp.exp(u), lambda a,f: compare(a,s,t,f),
                            (1, mp.exp(M('5.1')),mp.exp(8))))

    dyadic = tuple(exact(float(v)) for v in (1, mp.exp(M('5.1')),mp.exp(8)))
    (recentered,) = roots(lambda x: (compare(x,s,t,dyadic),), (mp.exp(u),))
    rows.append(report_pair("Burr held power 1 recentered", recentered,
                            lambda x,f: compare(x,s,t,f), dyadic))
    rows.extend(burr_routes())

    for kind in ("free", "held", "face"):
        crossing, compare, fixed = frechet_route(kind)
        rows.append(report_pair("Frechet "+kind, crossing, compare, fixed))

    a, c = roots(folded, ('2.61','7.2'))
    def fold_compare(c, _):
        (fit,) = roots(lambda f: (folded(f,c)[0],), (a,))
        assert mp.diff(lambda f: folded(f,c)[0], fit) < 0
        return folded(fit,c)[1]
    rows.append(report_pair("Foldnorm", c, fold_compare))

    b, a = roots(gpd, ('2.82','.0788'))
    def gpd_compare(a, _):
        (fit,) = roots(lambda f: (gpd(f,a)[0],), (b,))
        assert mp.diff(lambda f: gpd(f,a)[0], fit) > 0
        return gpd(fit,a)[1]
    rows.append(report_pair("GPD", a, gpd_compare))

    d = M(1)/1000
    def para_scores(a, b, r, d):
        lscore, lval = para(a, (d,r))
        hscore, hval = para(b, (d,r))
        return lscore, hscore, hval-lval
    a, b, r = roots(lambda a,b,r: para_scores(a,b,r,d), ('2.9','7400','.00336'))
    def para_compare(x, fixed):
        r, d = -mp.log(x), -mp.log(fixed[0])
        a1, b1 = roots(lambda a,b: para_scores(a,b,r,d)[:2], (a,b))
        for v in (a1,b1):
            assert mp.diff(lambda u: para(u,(d,r))[0], v) < 0
        return para_scores(a1,b1,r,d)[-1]
    rows.append(report_pair("Paralogis held scale", mp.exp(-r), para_compare, (mp.exp(-d),)))
    dyadic = (exact(float(mp.exp(-d))),)
    (recentered,) = roots(lambda x: (para_compare(x,dyadic),), (mp.exp(-r),))
    rows.append(report_pair("Paralogis held scale recentered", recentered,
                            para_compare, dyadic))
    return rows


if __name__ == "__main__":
    fixtures()
    mp.mp.dps = 85
    first = run()
    mp.mp.dps = 115
    second = run()
    for (label, fixed, rows), (check_label, check_fixed, check_rows) in zip(first, second):
        assert (label, fixed, rows.keys()) == (check_label, check_fixed, check_rows.keys())
        print(label, "fixed", *(x.hex() for x in fixed))
        for datum, gap in check_rows.items():
            assert abs(gap-rows[datum]) < M('1e-65')
            # Synthetic bounds for exercising policy, independent of replay accuracy.
            radius = M('1e-10')
            assert len(survivor_ids({"left": (-radius,radius),
                                    "right": (gap-radius,gap+radius)})) == 2
            radius = abs(gap)/4
            assert survivor_ids({"left": (-radius,radius),
                                 "right": (gap-radius,gap+radius)}) == {
                                     "right" if gap > 0 else "left"}
            print(datum, mp.nstr(gap, 17))
    print("All replay and policy assertions passed.")
