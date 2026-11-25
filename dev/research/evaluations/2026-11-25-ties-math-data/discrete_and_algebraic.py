"""Exact structural predicates and supplementary numerical tie replays."""
import math
from fractions import Fraction

import mpmath as mp
import sympy as sp

from replay import M, avg, exact, neighbors, roots, survivor_ids


def structural():
    def symmetric(xs):
        xs = sorted(map(Fraction, xs))
        return len({a+b for a, b in zip(xs, reversed(xs))}) == 1

    for sample in ((-1,0,0,0,0,0,1), (-2,)+(-1,)*10+(1,)*10+(2,),
                   (-1,0,1), (-100,-99,0,99,100)):
        assert symmetric(sample)
        changed = [*sample[:-1], math.nextafter(sample[-1], math.inf)]
        assert not symmetric(changed)
    upper = math.nextafter(1, math.inf)
    assert symmetric((1,upper))
    center = (Fraction(1)+Fraction(upper))/2
    assert center != Fraction(float(center))
    assert Fraction(1) < center < Fraction(upper)
    for scale, expected in ((math.nextafter(1,0), 2), (1., 1),
                            (math.nextafter(1,math.inf), 1)):
        assert (2 if Fraction(scale)**2 < 1 else 1) == expected
    for scale, feasible, interval in ((math.nextafter(2,0),False,False),
                                     (2.,True,False),
                                     (math.nextafter(2,math.inf),True,True)):
        assert (Fraction(scale) >= 2, Fraction(scale) > 2) == (feasible,interval)

    # Fatigue held shape a=3. The dyadic change preserves the prior
    # mod-seven obstruction and the inward score signs.
    b = sp.Symbol('b')
    eps = sp.Rational(1,2**42)
    assert pow(2,-42,7) == 1
    x = [sp.Integer(1),sp.Integer(1),1+eps]
    a = 3
    A = sum(x)/3
    H = 3 / sum(1/v for v in x)
    score = sum(v-b*b/v+a*a*b*(2*b/(b+v)-1) for v in x)/3
    p = sp.Poly(sp.fraction(sp.cancel(score))[0], b, domain=sp.QQ)
    inverse = sp.Poly(sp.expand(sum(k*(A*H)**i*b**(p.degree()-i)
                                    for (i,), k in p.terms())), b, domain=sp.QQ)
    assert score.subs(b,sp.Rational(1,2)) < 0 < score.subs(b,2)
    assert sp.gcd(p,inverse).degree() == 0
    print("Structural predicates and exact dyadic fatigue comparison passed.")


def skellam(counts, xs, seeds):
    d = avg(xs, counts)
    def bessel_sum(z):
        return avg([mp.log(mp.besseli(abs(x),z)) for x in xs], counts)

    def ell(t):
        z = 2*mp.sqrt(t*(t+d))
        return -(2*t+d)+d*mp.log((2*t+2*d)/z)+bessel_sum(z)

    def peak(seed):
        (t,) = roots(lambda t: (mp.diff(ell,t),), (seed,))
        assert mp.diff(ell,t,2) < 0
        return ell(t)
    right = peak(seeds[-1])
    if len(seeds) == 2:
        left = peak(seeds[0])
    else:
        left = -d+d*mp.log(d)-avg([mp.loggamma(x+1) for x in xs], counts)
    return right-left


def sstd(d, z):
    # The scale chart is z=M*s. F_- uses the left side up to z=1.
    skew = M(10)
    mean_offset = (skew-1/skew)*2*mp.sqrt(3)/mp.pi
    q = 3/(mean_offset*mean_offset)
    if z <= 1:
        low = (z-1)**2+q*z*z/skew**2
    else:
        low = ((z-1)**2+q*z*z*skew**2)/skew**4
    high = ((z+d)**2+q*z*z*skew**2)/(d*d)
    return -mp.log(low*high)/4


def policy_smoke(gap):
    fake = M('1e-10')
    assert survivor_ids({"left": (-fake,fake),
                         "right": (gap-fake,gap+fake)}) == {"left","right"}
    fine = abs(gap)/4
    assert survivor_ids({"left": (-fine,fine),
                         "right": (gap-fine,gap+fine)}) == {
                             "right" if gap > 0 else "left"}


def replay():
    for count, expected in [
        ((887_620_131,114_841_637_861,887_620_131), -1),
        ((7_163_054_741,926_766_879_000,7_163_054_741), 1),
    ]:
        gap = skellam(count, (-40,0,40), ('.672','3.39'))
        assert expected*gap > 0
        policy_smoke(gap)
        print("Skellam interior/interior", count, mp.nstr(gap,20))
    for count, expected in [
        ((10_228_536,2_749_660,538_344), -1),
        ((8_739_449,2_349_360,459_971), 1),
    ]:
        gap = skellam(count, (0,1,20), ('3.29',))
        assert expected*gap > 0
        policy_smoke(gap)
        print("Skellam interior/face", count, mp.nstr(gap,20))

    # At the collapse tie the finite local scale maximum is to the left
    # of the support join. Compare that branch with the collapse value.
    def equality(z,d):
        return mp.diff(lambda u: sstd(d,u),z), sstd(d,z)
    z,d = roots(equality, ('.9997','.02'))
    assert 0 < z < 1 and 0 < d < M('.5')
    for datum in neighbors(d):
        a = exact(datum)
        (fit,) = roots(lambda s: (mp.diff(lambda u: sstd(a,u),s),), (z,))
        gap = sstd(a,fit)
        assert 0 < abs(gap) < M('1e-10')
        assert mp.diff(lambda u: sstd(a,u),fit,2) < 0
        policy_smoke(gap)
        print("SSTD finite/collapse", datum.hex(), mp.nstr(gap,20))


if __name__ == "__main__":
    structural()
    for precision in (85,115):
        mp.mp.dps = precision
        print("decimal digits", precision, flush=True)
        replay()
    print("All supplementary assertions passed.")
