"""TASK-342: exact witness inequalities and 70-digit numerical replays."""
from fractions import Fraction as F

import mpmath as mp

mp.mp.dps = 70
TAU = mp.mpf(17)/16
EPS = mp.mpf(1)/1024


def exact_witnesses():
    # Machin's formula with five terms for atan(1/5) and two for atan(1/239).
    pi_hi = 16*sum((-1)**j*F(1, (2*j+1)*5**(2*j+1)) for j in range(5))
    pi_hi -= 4*(F(1, 239)-F(1, 3*239**3))
    assert pi_hi < F(314160, 100000)
    factorial = 1
    e_lo = F(1)
    for j in range(1, 10):
        factorial *= j
        e_lo += F(1, factorial)
    assert e_lo > F(271828, 100000)

    d2 = F(81, 4)
    variance = (99+d2)/200
    # exp(1/100) < 100/99.
    assert (2*F(271828, 314160)*variance)**100 > d2*F(100, 99)
    # exp(L(5,2)-C) > 2, using (nu,u) coordinates.
    ratio = F(8, 3)**200 * F(1, 2)**100 * F(2, 3)**297
    ratio *= (1+d2/2)**(-3)*d2
    assert ratio > 2
    # Maximum possible deficit at tau with epsilon <= 1.
    deficit = 101-F(201, 1)/(1+F(17, 16))
    assert deficit/(100-deficit) < F(1, 25)
    assert F(100, 10**5)+(7+F(1, 10**5))/1024 < F(8, 1000)
    print("Exact witness inequalities passed", flush=True)


def fit_u(nu, d, eps):
    d2 = d*d
    w = 99+eps
    target = (200+eps)/(nu+1)
    b = target*(1+d2)-w-d2
    c = (target-w-1)*d2
    delta = mp.sqrt(b*b-4*target*c)
    return -2*c/(delta+b) if b >= 0 else (delta-b)/(2*target)


def loglik(nu, u, d, eps):
    n = 200+eps
    b = mp.log(2/mp.sqrt(mp.pi))+mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)
    return n*(b-mp.log(u)/2)-(nu+1)*penalty(u, d, eps)/2


def penalty(u, d, eps):
    return (99+eps)*mp.log1p(1/u)+mp.log1p(d*d/u)


def profile(nu, d, eps):
    return loglik(nu, fit_u(nu, d, eps), d, eps)


def score(nu, d, eps):
    bprime = (mp.digamma((nu+1)/2)-mp.digamma(nu/2))/2
    return (200+eps)*bprime-penalty(fit_u(nu, d, eps), d, eps)/2


def normal(d, eps):
    n = 200+eps
    return n*(mp.log(2/mp.pi)-1-mp.log((99+eps+d*d)/n))/2


def collapse(d):
    return 200*mp.log(2/mp.pi)-2*mp.log(d)


def low(d, eps):
    return profile(mp.mpf(1), d, eps) if eps else collapse(d)


def scan(d, eps):
    grid = [TAU] + [mp.exp(mp.log(TAU)+i*(mp.log(10000)-mp.log(TAU))/240)
                    for i in range(1, 241)]
    roots = []
    last, prev = grid[0], score(grid[0], d, eps)
    for nu in grid[1:]:
        curr = score(nu, d, eps)
        if prev*curr < 0:
            root = mp.findroot(lambda v: score(v, d, eps), (last, nu), solver="anderson")
            assert last < root < nu
            roots.append((root, prev > 0))
        last, prev = nu, curr
    return roots


def replay(eps):
    left = mp.mpf(9)/2
    roots = scan(left, eps)
    print("epsilon", eps, "left score roots", [(mp.nstr(r, 22), m) for r, m in roots])
    maxima = [r for r, m in roots if m]
    assert maxima and max(profile(r, left, eps) for r in maxima) > low(left, eps) > normal(left, eps)
    guess = max(maxima, key=lambda r: profile(r, left, eps))
    nu, d = mp.findroot(
        lambda v, x: (score(v, x, eps), profile(v, x, eps)-low(x, eps)),
        (guess, mp.mpf("4.9")),
        tol=mp.mpf("1e-60"),
    )
    roots = scan(d, eps)
    print("crossing score roots", [(mp.nstr(r, 22), m) for r, m in roots])
    assert len(roots) == 2 and not roots[0][1] and roots[1][1]
    assert abs(roots[1][0]-nu) < mp.mpf("1e-50")
    assert low(d, eps)-normal(d, eps) > 0
    print("crossing d", mp.nstr(d, 45), "nu", mp.nstr(nu, 45))
    print("low minus normal", mp.nstr(low(d, eps)-normal(d, eps), 22),
          "interior sigma", mp.nstr(mp.sqrt(fit_u(nu, d, eps)/nu), 22))
    below, above = mp.mpf(float(d)), mp.mpf(float(d))
    import math
    if below > d:
        below = mp.mpf(math.nextafter(float(d), -math.inf))
    else:
        above = mp.mpf(math.nextafter(float(d), math.inf))
    for candidate in (below, above):
        winner = mp.findroot(lambda v: score(v, candidate, eps), nu)
        print("binary64", float(candidate).hex(),
              mp.nstr(profile(winner, candidate, eps)-low(candidate, eps), 18))
    right = mp.mpf(2)**512
    assert low(right, eps) > normal(right, eps)
    assert score(TAU, right, eps) < 0


if __name__ == "__main__":
    exact_witnesses()
    replay(mp.mpf(0))
    replay(EPS)
