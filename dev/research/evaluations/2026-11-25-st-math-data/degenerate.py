"""Symmetric singular seam and a finite/normal tie, at 65 decimal digits."""
import mpmath as mp

from check import first, logpdf, mills

B = mp.sqrt(2/mp.pi)


def singular():
    xs = [mp.mpf(0)]*10 + [mp.mpf(-1)]*9 + [mp.mpf(1)]*9 + [-mp.sqrt(6), mp.sqrt(6)]
    n = len(xs)
    assert all(abs(mp.fsum(x**k for x in xs)/n-m) < mp.eps*10
               for k, m in ((2, 1), (4, 3), (6, 15)))
    origin = mp.fsum(logpdf(x, 0, 0) for x in xs)
    k3 = B*(2*B*B-1)
    target = -n*k3*k3/12

    def profile(delta, h):
        a = delta/mp.sqrt(1-delta*delta)

        def objective(xi, l):
            return mp.fsum(logpdf((x-xi)*mp.exp(-l), a, h)-l for x in xs)

        def score(xi, l):
            if h == 0:
                z = [(x-xi)*mp.exp(-l) for x in xs]
                g = [-v+a*mills(a*v) for v in z]
                return -mp.exp(-l)*mp.fsum(g), -n-mp.fsum(v*d for v, d in zip(z,g))
            return (mp.diff(lambda t: objective(t,l), xi),
                    mp.diff(lambda t: objective(xi,t), l))

        init = (-B*delta/mp.sqrt(1-B*B*delta*delta), -mp.log1p(-B*B*delta*delta)/2)
        xi, l = mp.findroot(score, init)
        assert max(map(abs, score(xi,l))) < mp.mpf('1e-58')
        return objective(xi,l), mp.fsum(first((x-xi)*mp.exp(-l), a) for x in xs)

    for d in map(mp.mpf, ('0.02', '0.01', '0.005')):
        ll, slope = profile(d, mp.mpf(0))
        coeff = (ll-origin)/d**6
        assert abs(coeff-target) < 2*d*d
        assert abs(slope) < 100*d**4
        h = d*d
        gain = profile(d, h)[0]-origin
        assert abs(gain/h**2+n*mp.mpf('.75')) < 100*h
        print('singular delta', mp.nstr(d), 'face sextic, slope/delta^4, gain/h^2',
              *(mp.nstr(v, 16) for v in (coeff, slope/d**4, gain/h**2)))
    print('predicted sextic and df curvature coefficients', mp.nstr(target, 18), '-22.5')


def finite_tie():
    values = [mp.mpf(v)**2 for v in (1, 2, 3, 4, 99, 101)]
    mean = mp.fsum(values)/len(values)
    targets = [v/mean for v in values]
    n = 2*len(values)
    normal = n*logpdf(mp.mpf(1), 0, 0)

    def residual_squares(lam):
        return [1+lam*(v-1) for v in targets]

    def score(ss, h, u):
        return mp.fsum(v/(u+h*v) for v in ss)*(1+h)-len(ss)

    def value(ss, h, u):
        return 2*mp.fsum(logpdf(mp.sqrt(v/u), 0, h) for v in ss)-n*mp.log(u)/2

    def scale(ss, h):
        lower, upper = min(ss), max(ss)
        for _ in range(217):
            middle = (lower+upper)/2
            if score(ss, h, middle) > 0:
                lower = middle
            else:
                upper = middle
        return (lower+upper)/2

    def profile(lam, h):
        ss = residual_squares(lam)
        return value(ss, h, scale(ss, h))

    def stationary_crossing(lam, h, u):
        ss = residual_squares(lam)
        nu = 1/h
        d_norm = -(mp.digamma((nu+1)/2)-mp.digamma(nu/2)-1/nu)/(2*h*h)
        zs2 = [v/u for v in ss]
        derivative = n*d_norm+mp.fsum(
            (mp.log1p(h*v)-(1+h)*h*v/(1+h*v))/h**2 for v in zs2)
        return value(ss, h, u)-normal, derivative, score(ss, h, u)

    cauchy = mp.findroot(lambda l: profile(l, mp.mpf(1))-normal,
                         (mp.mpf('.85'), mp.mpf(1)))
    assert abs(profile(cauchy, mp.mpf(1))-normal) < mp.mpf('1e-59')
    hs = [mp.mpf(j)/100 for j in range(1,101)]
    gain, h_start = max((profile(cauchy, h)-normal,h) for h in hs)
    assert gain > 0 and h_start < 1
    print('Cauchy crossing lambda, better grid witness h and gain',
          *(mp.nstr(v, 18) for v in (cauchy, h_start, gain)))
    lam, h, u = mp.findroot(stationary_crossing,
                            (cauchy, h_start, scale(residual_squares(cauchy), h_start)))
    assert 0 < lam < cauchy and 0 < h < 1 and u > 0
    assert max(map(abs, stationary_crossing(lam, h, u))) < mp.mpf('1e-58')
    assert max(profile(lam, v)-normal for v in hs) < 0
    print('interior stationary crossing lambda, h, scale',
          *(mp.nstr(v, 25) for v in (lam, h, mp.sqrt(u))))
    for offset in map(mp.mpf, ('-0.000001','0.000001')):
        gains = [profile(lam+offset, v)-normal for v in hs+[h]]
        assert (max(gains) > 0) == (offset > 0)
        print('tie path offset, max gain on positive h grid and stationary crossing h',
              mp.nstr(offset), mp.nstr(max(gains), 16))


if __name__ == '__main__':
    singular()
    finite_tie()
