from fractions import Fraction as F
from itertools import combinations_with_replacement
from math import factorial, fsum, hypot

import mpmath as mp
import sympy as sp


def ratio(k, z2, depth):
    """Even depth gives a lower bound, odd depth an upper bound."""
    value = 0
    for j in range(depth - 1, -1, -1):
        value = z2 / (2 * (k + j + 1) + value)
    return value


def stationary_face():
    t = sp.Symbol("t")
    z2 = 4 * t * (t + 2)
    for depth in [3, 5, 9, 17, 33]:
        ratios = [sp.cancel(ratio(k, z2, depth)) for k in (0, 3)]
        numerator, denominator = sp.fraction(
            sp.cancel(6 * t - ratios[0] - 2 * ratios[1])
        )
        n_poly, d_poly = sp.Poly(numerator, t), sp.Poly(denominator, t)
        assert all(c >= 0 for c in d_poly.all_coeffs()) and d_poly.TC() > 0
        assert n_poly.nth(0) == n_poly.nth(1) == 0
        p = sp.Poly(numerator / t**2, t)
        degree = p.degree()
        # Coefficients after t = 16*y/(1+y), clearing (1+y)**degree.
        coefficients = [
            sum(p.nth(i) * 16**i * sp.binomial(degree - i, j - i)
                for i in range(j + 1))
            for j in range(degree + 1)
        ]
        if min(coefficients) > 0:
            print("stationary face exact polynomial", depth, degree,
                  min(coefficients), d_poly.TC())
            return
    raise AssertionError("stationary-face polynomial sign unresolved")


def nonunimodal():
    d = F(1)
    counts = [(0, 19), (20, 1)]
    for t, depth, threshold in [(F(1, 2), 2, F(271, 6787)), (F(32), 101, F("-0.3570"))]:
        z2 = 4 * t * (t + d)
        bound = sum(w * (k + ratio(k, z2, depth)) for k, w in counts) / 20 - (2*t+d)
        assert bound == threshold if depth % 2 == 0 else bound < threshold
        print("exact log-argument score bound", t, depth, float(bound))
    assert d * (19 + F(1, 21)) / 20 - 1 == -F(1, 21)
    # Taylor upper bound for e: beyond 1/6!, consecutive ratios are <=1/7.
    e_upper = sum(F(1, factorial(j)) for j in range(6)) + F(7, 6*factorial(6))
    assert e_upper < F(68, 25)
    assert (F(3, 2)/F(68, 25))**20 * F(121, 64)**19 > 1

    # Replicate the one-sided witness and change a single zero to -1.
    weights = [(0, 1899), (1, 1), (20, 100)]
    d = F(1999, 2000)
    assert sum(k*w for k,w in weights)/F(2000)-d == F(1, 1000)
    cells = [(F(1, 20), 3, F("-0.001600")), (F(1, 2), 2, F("0.04046")),
             (F(32), 101, F("-0.3570"))]
    for t, depth, threshold in cells:
        z2 = 4*t*(t+d)
        bound = sum(w*(k+ratio(k,z2,depth)) for k,w in weights)/2000 - (2*t+d)
        assert bound > threshold if depth % 2 == 0 else bound < threshold
        print("two-interior-modes exact score bound", t, depth, float(bound))

    m = mp.mp
    for t in map(m.mpf, ["0.05", "0.5", "32"]):
        z = 2*m.sqrt(t*(1+t))
        h = sum(w*z*m.besseli(k+1, z)/m.besseli(k,z) for k,w in counts)/20
        print("numerical log-argument score", t, m.nstr(h-2*t, 30))
    t = m.mpf("0.5")
    z = 2*m.sqrt(t*(1+t))
    gain = sum(w*(-2*t + k*m.log(1+t) + m.log(m.hyp0f1(k+1,z*z/4)))
               for k,w in counts)/20
    assert gain > 0
    print("numerical gain per observation", m.nstr(gain, 30))


def scan():
    # Search an integer grid for more than one positive-to-negative crossing.
    m = mp.mp
    grid = [m.exp(m.mpf(j)/4) for j in range(-32, 29)]
    ratios = [[z*m.besseli(k+1,z)/m.besseli(k,z) + k for k in range(13)]
              for z in grid]
    samples = multiple = 0
    for support in combinations_with_replacement(range(13), 3):
        for weights in [(8, 1, 1), (1, 8, 1), (1, 1, 8), (1, 1, 1)]:
            d = m.mpf(sum(x*w for x,w in zip(support,weights)))/sum(weights)
            scores = [sum(w*h[k] for k,w in zip(support,weights))/sum(weights)
                      - m.sqrt(z*z+d*d) for z,h in zip(grid,ratios)]
            multiple += sum(a > 0 > b for a,b in zip(scores,scores[1:])) > 1
            samples += 1
    print("numerical profile scan", samples, len(grid), multiple)


def ordinary():
    """Strict cover against a feasible point, with the face as origin."""
    iv, m = mp.iv, mp.mp
    iv.dps = m.dps

    def interval(a, b=None):
        return iv.mpf([str(a), str(a if b is None else b)])

    for x, a, k, b, bracket in [
        ("near-face", 17, 4, 161, ("0.00005", "0.00007")),
        ("competing face", 19, 20, 1, ("6", "14")),
    ]:
        n = a+b
        counts = [(0, a), (k, b)]
        d = iv.mpf(k*b)/n
        d_mp = m.mpf(k*b)/n

        def derivatives(t, depth):
            z2 = 4*t*(t+d)
            def a_k(k):
                # A_k decreases with q. Bound it at the two endpoints.
                lower = interval(2)/(2*(k+1)+ratio(k+1,z2.b,depth+1))
                upper = interval(2)/(2*(k+1)+ratio(k+1,z2.a,depth))
                return iv.mpf([lower.a, upper.b])
            score, slope = -2+d/(d+t), -d/(d+t)**2
            for k, weight in counts:
                v = a_k(k)
                score += (d+2*t)*v*weight/n
                slope += (2*v+(d+2*t)**2*v*(a_k(k+1)-v))*weight/n
            return score, slope

        def value(t):
            q = t*(t+d)
            gain = -2*t+d*iv.ln(1+t/d)
            for k, weight in counts:
                term = total = interval(1)
                for j in range(1, 19200):
                    term *= q/(j*(j+k))
                    total += term
                    p = q/((j+1)*(j+k+1))
                    if p.b < interval("0.5") and (term/total).b < interval("1e-40"):
                        remainder = term*p/(1-p)
                        total += interval(0,1)*remainder
                        break
                else:
                    raise AssertionError("series cap")
                gain += iv.ln(total)*weight/n
            return gain

        def score_mp(t):
            q = t*(t+d_mp)
            score = -2+d_mp/(d_mp+t)
            for k, weight in counts:
                score += (d_mp+2*t)*m.hyp0f1(k+2,q)/m.hyp0f1(k+1,q)*weight/(n*(k+1))
            return score

        center = m.findroot(score_mp, tuple(map(m.mpf,bracket)))
        radius = center*m.mpf("0.04")
        j_lo, j_hi = center-radius, center+radius
        local = [(j_lo+2*radius*i/256, j_lo+2*radius*(i+1)/256) for i in range(256)]
        for l, r in local:
            assert derivatives(interval(l,r), 80)[1].b < 0
        l, r = center*(1-m.mpf("4e-11")), center*(1+m.mpf("4e-11"))
        assert derivatives(interval(l), 80)[0].a > 0 > derivatives(interval(r), 80)[0].b
        lower = value(interval(center)).a
        assert lower > 0
        face = -d+b*(k*iv.ln(d)-iv.ln(factorial(k)))/n
        tail = 1
        while (-iv.ln(2*iv.pi*tail)/2-face).b >= lower:
            tail *= 2

        todo = [(m.mpf(0), j_lo), (j_hi, m.mpf(tail))]
        examined = 0
        M = 2*sum(F(w, n*(k+1)) for k,w in counts)
        M = iv.mpf(M.numerator)/M.denominator
        while todo:
            l, r = todo.pop()
            mid, half = (l+r)/2, (r-l)/2
            z = interval(mid)
            # A midpoint likelihood and zero-order score suffice outside J.
            g, _ = derivatives(z, min(19200, max(30, 2*int(mid)+20)))
            upper = value(z)+abs(g)*interval(half)+M*interval(half)**2/2
            examined += 1
            assert examined <= 19200
            if upper.b >= lower:
                todo.extend([(l, mid), (mid, r)])
        print("ordinary strict certificate", x, "t", m.nstr(center, 20),
              "cover cells", examined, "tail starts at", tail)


def wide():
    m, iv = mp.mp, mp.iv
    iv.dps = m.dps
    data = [-8, -7, 992, 999992]
    orders = list(map(abs, data))
    d = sum(data) / F(4)
    d2 = d*d

    def interval(a, b=None):
        return iv.mpf([str(a), str(a if b is None else b)])

    # The mean and every z endpoint used below are terminating decimals.
    def decimal(r):
        return m.nstr(m.mpf(r.numerator)/r.denominator, m.dps)

    d_i = interval(decimal(d))

    def bounds(z):
        z2 = z*z
        H = iv.sqrt(z2+d_i*d_i)
        score = slope = interval(0)
        for k in orders:
            R = iv.sqrt(z2+k*k)
            # Both fractions keep all large-rate cancellations symbolic.
            delta = (k*k-d_i*d_i)/(R+H)
            w = k/z
            q = 1/(1+w*w)
            b = q*(4-5*q)/8
            bp = (2-5*q)/4
            e = 5/z2
            E = (2*q*(1-q)*bp-2*q*b)/R+b*b/(R*R)
            score += delta-q/2+b/R+interval(-1, 1)*e
            slope += (-delta*(z/R)*(z/H) - q*(1-q)
                      + (2*q*(1-q)*bp-q*b)/R - E
                      + interval(-1, 1)*2*R*e)
        return score/4, slope/4

    assert F(sum(orders), 4) - d - F(1000**2, 2*d) - 1 > 0
    lo, bins = 1000, 0
    K = max(orders)
    while lo < 10**7:
        hi = min(2*lo, 10**7)
        Hlo = iv.sqrt(interval(lo**2)+d_i*d_i)
        numerator = K*K-16*d_i*d_i+24*d_i*d_i*lo/(Hlo+lo)
        # R_K + 4H is an upper bound on R_K+4H-3z.
        denominator = 4*(iv.sqrt(interval(hi**2+K*K)) +
                         4*iv.sqrt(interval(hi**2)+d_i*d_i))
        assert (numerator/denominator).a > 1
        lo, bins = hi, bins+1
    assert lo == 10**7
    # R_k+z <= 2.01*z in this range. Bound the correction from below.
    c = F(sum(k*k for k in orders), 4)/F("2.01") - d2/2
    assert c/10**11 > F(1, 2)+F(1, 8*10**7)+F(5, 10**14)
    # For z >= 4e11, q_k >= 0.99 and R_k >= z.
    tail = F(sum(k*k for k in orders), 4)/(8*10**11)
    assert tail-F("0.495")+F(1, 32*10**11)+F(5, (4*10**11)**2) < 0

    # Interval expressions over narrower subranges preserve z/R and z/H.
    slopes = []
    for i in range(100, 400, 10):
        _, slope = bounds(interval(i*10**9, (i+10)*10**9))
        assert slope.b < -iv.mpf("0.19")
        slopes.append(slope.b)
    print("wide lower-region mesh cells", bins, "curvature cells", len(slopes),
          "maximum curvature upper bound", max(slopes))

    # Isolate with score bounds only, checking a binary64-sized practical pad.
    lo, hi = 10**11, 4*10**11
    pad = interval("0.0000000000001")
    evaluations = 0
    while (hi-lo) * 10**11 > lo:
        mid = (lo+hi)//2
        score, _ = bounds(interval(mid))
        evaluations += 1
        if score.a > pad:
            lo = mid
        elif score.b < -pad:
            hi = mid
        else:
            for endpoint, sign in [(mid-1, 1), (mid+1, -1)]:
                score, _ = bounds(interval(endpoint))
                assert (sign*score).a > pad
            lo, hi = mid-1, mid+1
    print("wide z bracket", lo, hi, "bisection evaluations", evaluations)
    estimates = [(m.sqrt(m.mpf(lo+hi)**2/4 + m.mpf(decimal(d))**2) + sign*m.mpf(decimal(d)))/2
                 for sign in (1,-1)]
    print("wide coefficient estimates", *[m.nstr(v, 25) for v in estimates])

    # Check a cancellation-free binary64 evaluation at the final endpoints.
    def score64(z):
        d64 = float(d)
        H = hypot(z, d64)
        terms = []
        for k in orders:
            R = hypot(z, k)
            q = 1/(1+(k/z)**2)
            delta = ((k-d64)*(k+d64))/(R+H)
            terms.append(delta-q/2+q*(4-5*q)/(8*R))
        return fsum(terms)/4

    values = [score64(lo), score64(hi)]
    assert values[0] > 1e-13 and values[1] < -1e-13
    for endpoint, estimate in zip([lo, hi], values):
        score, _ = bounds(interval(endpoint))
        assert (abs(score-interval(estimate))).b < pad
    for sign in (1, -1):
        rate = lambda z: (iv.sqrt(interval(z)**2+d_i*d_i)+sign*d_i)/2
        assert (rate(hi)/rate(lo)-1).b < iv.mpf("1e-10")
    print("wide binary64 bracket scores", *values)

    mid = (lo+hi)//2
    score, _ = bounds(interval(mid))
    for endpoint in (10**11, 4*10**11):
        distance = iv.ln(interval(endpoint)/mid)
        upper = score*distance - iv.mpf("0.095")*distance*distance
        assert upper.b < 0
        print("wide relative log likelihood endpoint upper bound", endpoint, upper)
    # Independent Bessel evaluation close to the root and on either side.
    d_mp = m.mpf(decimal(d))
    for z in [lo, mid, hi, 10**11, 4*10**11]:
        z = m.mpf(z)
        H = m.sqrt(z*z+d_mp*d_mp)
        h = [k+z*m.besseli(k+1,z)/m.besseli(k,z) for k in orders]
        score_ref = sum(h)/4-H
        slope_ref = sum(z*z+k*k-v*v for k,v in zip(orders,h))/4-z*z/H
        score_i, slope_i = bounds(interval(int(z)))
        for ref, enclosure in [(score_ref, score_i), (slope_ref, slope_i)]:
            assert iv.mpf(m.nstr(ref, 50)) in enclosure
    print("wide independent Bessel point checks", 5)


if __name__ == "__main__":
    mp.mp.dps = 60
    stationary_face()
    nonunimodal()
    scan()
    ordinary()
    wide()
    print("all checks passed")
