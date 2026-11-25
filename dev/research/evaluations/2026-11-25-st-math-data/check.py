"""High-precision checks for the skew-t normal seam. Run from the repository root."""
from fractions import Fraction
import json
from pathlib import Path

import mpmath as mp

mp.mp.dps = 65
SQRT2 = mp.sqrt(2)
LOG2PI = mp.log(2 * mp.pi)


def cdf(x):
    return mp.erfc(-x / SQRT2) / 2


def mills(x):
    return mp.exp(-(x*x + LOG2PI)/2) / cdf(x)


def tcdf(x, nu):
    if x == 0:
        return mp.mpf('.5')
    if x*x < 64 or nu > 1000:
        centered = mp.betainc(mp.mpf('.5'), nu/2, 0, x*x/(nu+x*x),
                              regularized=True)/2
        return mp.mpf('.5') + mp.sign(x)*centered
    tail = mp.betainc(nu/2, mp.mpf('.5'), 0, nu/(nu+x*x), regularized=True)/2
    return tail if x < 0 else 1-tail


def logpdf(z, alpha, h):
    if h == 0:
        return -(LOG2PI+z*z)/2 + mp.log(2*cdf(alpha*z))
    nu = 1/h
    return (mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(mp.pi*nu)/2
            -(nu+1)*mp.log1p(h*z*z)/2
            +mp.log(2*tcdf(alpha*z*mp.sqrt((nu+1)/(nu+z*z)), nu+1)))


def first(z, alpha):
    t = alpha*z
    return (z**4-2*z*z-1 + mills(t)*t*(1-2*z*z-t*t))/4


def second(z, alpha):
    def mixture(w):
        return mp.sqrt(w)*mp.exp(-z*z*w/2)*cdf(alpha*z*mp.sqrt(w))
    value, _, d2, d3, d4 = mp.diffs(mixture, 1, 4)
    return (4*d3/3 + d4/2)/value - (d2/value)**2/2


def transition():
    def face_alpha(zs):
        return mp.findroot(lambda a: sum(z*mills(a*z) for z in zs), (0, 1))
    def slope(s):
        zs = (-s, 2*s)
        a = face_alpha(zs)
        return sum(first(z, a) for z in zs)
    critical = mp.findroot(slope, (1, mp.mpf('1.2')))
    assert abs(slope(critical)) < mp.mpf('1e-60')
    print('asymmetric scale transition', mp.nstr(critical, 26))
    for delta in ('-0.000001', '0', '0.000001'):
        zs = (-critical-mp.mpf(delta), 2*(critical+mp.mpf(delta)))
        a = face_alpha(zs)
        face = sum(logpdf(z, a, 0) for z in zs)
        A = slope(-zs[0])
        H = mp.diff(lambda v: sum(logpdf(z, v, 0) for z in zs), a, 2)
        g = mp.diff(lambda v: sum(first(z, v) for z in zs), a)
        D = sum(second(z, a) for z in zs) - g*g/(2*H)
        assert D < 0
        print(' asym', delta, 'alpha, slope, curvature coefficient',
              *(mp.nstr(v, 16) for v in (a, A, D)))
        gains = []
        for h in map(mp.mpf, ('1e-3', '1e-4', '1e-6', '1e-7', '1', '.5', '.1', '.01')):
            nu = 1/h
            qs = [z*mp.sqrt((nu+1)/(nu+z*z)) for z in zs]
            c = mp.exp(mp.loggamma((nu+2)/2)-mp.loggamma((nu+1)/2)
                       -mp.log(mp.pi*(nu+1))/2)
            def score(v):
                return sum(q*c*(1+(v*q)**2/(nu+1))**(-(nu+2)/2)
                           /tcdf(v*q, nu+1) for q in qs)
            # The analytically unique positive root is bracketed by its score signs.
            left, right = mp.mpf(0), mp.mpf(1)
            while score(right) > 0:
                right *= 2
            opt = mp.findroot(score, (left, right), solver='anderson')
            assert opt > 0 and abs(score(opt)) < mp.mpf('1e-57')
            gain = sum(logpdf(z, opt, h) for z in zs)-face
            if h <= mp.mpf('.001'):
                assert abs(gain-h*A-h*h*D) < 100*h**3
            if delta != '0.000001':
                assert gain < 0
            gains.append(gain)
        if delta == '0.000001':
            assert gains[3] > 0
        print('  profile gains', *(mp.nstr(v, 10) for v in gains))

    # Exact rational lower bound on the integral in the critical symmetric proof.
    a = Fraction(12, 5)
    integral = (a**3/3-a*a/2+Fraction(1, 6))/Fraction(7, 2)-Fraction(1, 6)
    assert integral > Fraction(1, 12)
    sc = 1+mp.sqrt(2)
    for delta in map(mp.mpf, ('-0.000001', '0', '0.000001')):
        z = mp.sqrt(sc+delta)
        gains = [2*(logpdf(z, 0, h)-logpdf(z, 0, 0))
                 for h in map(mp.mpf, ('1', '.1', '.001', '1e-6', '1e-7', '1e-8'))]
        assert (max(gains) > 0) == (delta > 0)
        print(' symmetric', mp.nstr(delta), *(mp.nstr(v, 10) for v in gains))


def seam_grid():
    worst = mp.mpf(0)
    for z in map(mp.mpf, ('-4', '-2', '-.5', '0', '.5', '2', '4')):
        for a in map(mp.mpf, ('-3', '-1', '0', '1', '3')):
            A, B = first(z, a), second(z, a)
            if a == 0:
                assert abs(B-(z**4/4-z**6/6)) < mp.mpf('1e-57')
            errors = []
            for h in map(mp.mpf, ('1e-4', '1e-5', '1e-6')):
                err = abs(logpdf(z, a, h)-logpdf(z, a, 0)-h*A-h*h*B)/h**3
                errors.append(err)
                worst = max(err, worst)
            assert abs(errors[1]-errors[2]) < mp.mpf('.03')*(1+errors[2])
    assert worst < 10**8
    print('105 pointwise seam probes, max |remainder|/h^3', mp.nstr(worst, 15))


def half_t_upper(xs):
    n = len(xs)
    cn = mp.log(2)/2-mp.log(mp.pi)/2
    upper = []
    for ys in (xs, [-x for x in xs]):
        distances = sorted(x-min(ys) for x in ys)
        bounds = []
        for j in range(1, (n+1)//2):
            d, p = distances[j], n-j
            w = d*mp.sqrt((mp.mpf(2*p)/n-1)/2)
            bounds.append(n*(cn-mp.log(w))-p*mp.log1p(d*d/(2*w*w)))
        upper.append(min(bounds))
    return upper


def sn_upper(xs, target):
    n = len(xs)
    origin = mp.fsum(xs)/n
    xs = [x-origin for x in xs]
    ss = mp.fsum(x*x for x in xs)
    cn = n*(mp.log(2)-LOG2PI/2)
    cutoff = mp.mpf(8)
    # Penalize all wrong-side observations in a piecewise quadratic.
    def wing(ys):
        # Retain the extreme observations selected at the minimizing location.
        ys = sorted(ys)
        q = ss
        for count in range(1, n):
            selected = ys[:count]
            total = mp.fsum(selected)
            center = cutoff**2*total/(n+cutoff**2*count)
            if ys[count-1] <= center <= ys[count]:
                q = ss+n*center**2 + cutoff**2*mp.fsum((x-center)**2 for x in selected)
                break
        assert q > ss
        return cn+n*(mp.log(n/q)-1)/2

    wings = [wing(xs), wing([-x for x in xs])]
    assert max(wings) < target
    pending = [(-cutoff, cutoff)]
    feasible = (-mp.inf, None)
    terminal = []
    steps = 0
    while pending:
        left, right = pending.pop()
        a = (left+right)/2
        # Any anchor gives a valid tangent bound. Damped Newton is only to
        # tighten the anchor and produce a useful feasible point.
        eta, b = mp.sqrt(n/ss), -a/(1+abs(a))/2

        def value(u, v):
            zs = [u*x-v for x in xs]
            return cn+n*mp.log(u)-mp.fsum(z*z for z in zs)/2+mp.fsum(
                mp.log(cdf(a*z)) for z in zs)

        ll = value(eta, b)
        for _ in range(16):
            zs = [eta*x-b for x in xs]
            rs = [mills(a*z) for z in zs]
            gs = [-z+a*r for z, r in zip(zs, rs)]
            ws = [1+a*a*r*(a*z+r) for z, r in zip(zs, rs)]
            g1, g2 = n/eta+mp.fsum(x*g for x, g in zip(xs, gs)), -mp.fsum(gs)
            if max(abs(g1), abs(g2)) < mp.mpf('1e-20'):
                break
            h11 = n/eta**2+mp.fsum(x*x*w for x, w in zip(xs, ws))
            h12 = -mp.fsum(x*w for x, w in zip(xs, ws))
            h22 = mp.fsum(ws)
            det = h11*h22-h12*h12
            d1, d2 = (h22*g1-h12*g2)/det, (h11*g2-h12*g1)/det
            for j in range(16):
                u, v = eta+d1/2**j, b+d2/2**j
                if u > 0:
                    trial = value(u, v)
                    if trial > ll:
                        eta, b, ll = u, v, trial
                        break
            else:
                break
        if ll > feasible[0]:
            feasible = (ll, (origin+b/eta, 1/eta, a))
        ts = [a*(eta*x-b) for x in xs]
        rs = [mills(t) for t in ts]
        intercept = cn+mp.fsum(mp.log(cdf(t))-r*t for t, r in zip(ts, rs))
        q, s = mp.fsum(x*r for x, r in zip(xs, rs)), mp.fsum(rs)

        def upper(v):
            d = v*q
            root = mp.sqrt(d*d+4*n*ss)
            u = (root+d)/(2*ss) if d >= 0 else 2*n/(root-d)
            w = -v*s/n
            return intercept+n*mp.log(u)-(ss*u*u+n*w*w)/2+v*(u*q-w*s)

        bound = max(upper(left), upper(right))
        assert ll <= bound
        steps += 1
        assert steps < 512
        if bound < target:
            terminal.append(bound)
        else:
            pending.extend(((left, a), (a, right)))
    return max(terminal+wings), feasible, steps, wings


def golden():
    path = Path('univariateML-cpp/include/cppunivariate/distributions/st/golden.json')
    data = json.loads(path.read_text())
    xs = list(map(mp.mpf, data['data']))
    xi, w, a, nu = (mp.mpf(data['estimates'][key]) for key in ('xi', 'omega', 'alpha', 'nu'))
    ll = mp.fsum(logpdf((x-xi)/w, a, 1/nu)-mp.log(w) for x in xs)
    assert abs(ll-data['logLik']) < mp.mpf('1e-10')
    half = half_t_upper(xs)
    upper, (face, point), steps, wings = sn_upper(xs, ll-2)
    assert max(upper, *half) < ll-2
    print('golden witness', mp.nstr(ll, 25), 'half-t upper bounds', *(mp.nstr(v, 16) for v in half))
    print('sn upper', mp.nstr(upper, 18), 'wing bounds', *(mp.nstr(v, 16) for v in wings),
          'boxes', steps)
    # Refine the best eligible tangent anchor as a stationary face point.
    def objective(m, l, skew):
        return mp.fsum(logpdf((x-m)*mp.exp(-l), skew, 0)-l for x in xs)
    start = (point[0], mp.log(point[1]), point[2])
    def score(m, l, skew):
        zs = [(x-m)*mp.exp(-l) for x in xs]
        rs = [mills(skew*z) for z in zs]
        gs = [-z+skew*r for z, r in zip(zs, rs)]
        return (-mp.exp(-l)*mp.fsum(gs), -len(xs)-mp.fsum(z*g for z, g in zip(zs, gs)),
                mp.fsum(z*r for z, r in zip(zs, rs)))
    m, l, skew = mp.findroot(score, start, maxsteps=70)
    assert max(map(abs, score(m, l, skew))) < mp.mpf('1e-55')
    print('sn stationary point xi omega alpha LL A',
          *(mp.nstr(v, 17) for v in (m, mp.exp(l), skew, objective(m,l,skew),
                                     mp.fsum(first((x-m)*mp.exp(-l), skew) for x in xs))))
    print('best visited face anchor', mp.nstr(face, 16))


if __name__ == '__main__':
    seam_grid()
    transition()
    golden()
