"""Exact rational grids and tail certificates for held and free urn sizes."""
from fractions import Fraction as Q
from itertools import combinations_with_replacement
from math import comb, prod
from random import Random

import mpmath as mp
from sympy import Poly, symbols

from explore import black_ratio, white_ratio

N = symbols('N')


def falling(x, k):
    return prod(x-i for i in range(k))


def reduced(r, ys, m, n):
    return Q(falling(m, r)**len(ys) * prod(falling(n, y) for y in ys),
             prod(falling(m+n, r+y) for y in ys))


def independent(r, ys, m, n):
    return prod(Q(r*comb(m, r)*comb(n, y), (r+y)*comb(m+n, r+y))
                for y in ys)


def face(r, ys):
    p = Q(len(ys)*r, len(ys)*r+sum(ys))
    return p**(len(ys)*r)*(1-p)**sum(ys)


def best(r, ys, population):
    return max((reduced(r, ys, m, population-m), m)
               for m in range(r, population-max(ys)+1))


def tail(r, ys):
    h, s = len(ys), sum(ys)
    p = Q(h*r, h*r+s)
    q = 1-p
    a, d = p.numerator, p.denominator
    gprime = (h*r*(r-1)/p**2 - sum(y*(y-1) for y in ys)/q**2)/2
    information = h*r/p**2+s/q**2
    denominator = Poly(prod(falling(N, r+y) for y in ys), N)
    face_scaled = a**(h*r)*(d-a)**s
    polys = []
    profiles = []
    threshold = r+max(ys)
    for residue in range(d):
        f = (p*residue) % 1
        k = (gprime/information+f-Q(1, 2)) // 1
        for j in (k-1, k+1):
            c = int((j-f)*d)
            m, n = a*N+c, (d-a)*N-c
            threshold = max(threshold, (d*r-c+a-1)//a,
                            (d*(max(ys)+1)+c+d-a-1)//(d-a))
            numerator = (m+d)**h * prod(n-d*y for y in ys)
            divisor = (m+d*(1-r))**h * n**h
            polys.append((Poly(numerator-divisor, N), 1 if j == k-1 else -1))
        for j in (k, k+1):
            c = int((j-f)*d)
            numerator = prod(a*N+c-d*i for i in range(r))
            numerator = numerator**h * prod((d-a)*N-c-d*i for y in ys for i in range(y))
            diff = Poly(numerator, N)-face_scaled*denominator
            assert diff
            polys.append((diff, None))
            profiles.append((residue, j, diff))
    while any(any(coef*poly.LC() < 0 for coef in poly.shift(threshold).all_coeffs())
              for poly, _ in polys):
        threshold *= 2
    assert all(expected is None or (poly.LC() > 0 if expected > 0 else poly.LC() < 0)
               for poly, expected in polys)
    return profiles, threshold+1


def certify(r, ys):
    F = face(r, ys)
    records, threshold = tail(r, ys)
    p = Q(len(ys)*r, len(ys)*r+sum(ys))
    signs = []
    for residue, j, poly in records:
        population = threshold+(residue-threshold) % p.denominator
        m = int(p*population)+j
        value = reduced(r, ys, m, population-m)
        sign = (value > F)-(value < F)
        assert sign == (int(bool(poly.LC() > 0))-int(bool(poly.LC() < 0)))
        biggest = best(r, ys, population)[0]
        residue_values = [
            reduced(r, ys, int(p*population)+offset, population-int(p*population)-offset)
            for other_residue, offset, _ in records if residue == other_residue
        ]
        assert biggest == max(residue_values)
        signs.append(sign)
    finite = [best(r, ys, v)[0] for v in range(r+max(ys), threshold)]
    finite_best = max(finite, default=Q(0))
    sign = max(max(signs), (finite_best > F)-(finite_best < F))
    return sign, threshold, max(signs), finite_best/F


def exact_profiles():
    checks = [0, 0]
    for r in range(1, 4):
        for h in range(1, 5):
            for ys in combinations_with_replacement(range(6), h):
                for held in range(r, 8):
                    pairs = [(held, n) for n in range(max(ys), 45)]
                    verify_line(r, ys, pairs, white_ratio)
                    checks[0] += 1
                if sum(ys):
                    for held in range(max(ys), 8):
                        pairs = [(m, held) for m in range(r, max(45, held*r*h//sum(ys)+3))]
                        verify_line(r, ys, pairs, black_ratio)
                        checks[1] += 1
    print('Exact profile grids (white, black):', checks, flush=True)


def verify_line(r, ys, pairs, ratio):
    ls = [independent(r, ys, m, n) for m, n in pairs]
    ratios = [ratio(m, n, r, ys) for m, n in pairs[:-1]]
    assert all(b == a*q for a, q, b in zip(ls, ratios, ls[1:]))
    assert all(b < 1 for a, b in zip(ratios, ratios[1:]) if a <= 1)
    first = next(i for i, q in enumerate(ratios) if q <= 1)
    expected = [first, first+1] if ratios[first] == 1 else [first]
    peak = max(ls)
    assert [i for i, v in enumerate(ls) if v == peak] == expected


def high_precision():
    rng = Random(325)
    mp.mp.dps = 80
    checked = 0
    for _ in range(150):
        r = rng.randint(1, 12)
        ys = tuple(rng.randint(0, 25) for _ in range(rng.randint(2, 10)))
        if not sum(ys):
            continue
        h, s = len(ys), sum(ys)
        p = mp.mpf(h*r)/(h*r+s)
        q = 1-p
        b = mp.mpf(s)/h
        variance = sum((y-b)**2 for y in ys)/h
        g = h*(r+b-r*variance/b)/2
        gp = (h*r*(r-1)/p**2-sum(y*(y-1) for y in ys)/q**2)/2
        information = h*r/p**2+s/q**2
        pop = 10**24+rng.randrange(1000)
        m = int(p*pop)+rng.randint(-30, 30)
        delta = m-p*pop
        n = pop-m

        def log_reduced(white, black):
            def log_falling(z, k):
                return mp.fsum(mp.log(z-i) for i in range(k))
            return (h*log_falling(white, r)+mp.fsum(log_falling(black, y) for y in ys)
                    -mp.fsum(log_falling(pop, r+y) for y in ys))

        value = log_reduced(m, n)
        face_log = h*r*mp.log(p)+s*mp.log(q)
        assert abs(pop*(value-face_log)-g) < mp.mpf('1e-13')
        assert abs(pop**2*(log_reduced(m+1, n-1)-value)
                   -(gp-information*(delta+mp.mpf('0.5')))) < mp.mpf('1e-13')
        checked += 1
    print(f'80-digit expansions: {checked} seeded trials (325), N in [10^24, 10^24+999]',
          flush=True)


def joint_examples():
    cases = [
        ((0, 1), 1), ((0, 3), 1), ((0, 0, 0, 2), -1),
        ((0, 0, 3), 1), ((0, 0, 0, 5), -1), ((0, 0, 2), 1),
    ]
    for ys, expected in cases:
        sign, threshold, tail_sign, finite_ratio = certify(1, ys)
        assert sign == expected
        print('Joint exact (ys, sign, cutoff, max tail sign, finite-prefix/face):',
              ys, sign, threshold, tail_sign, finite_ratio, flush=True)
    ys = (0, 0, 3)
    ratios = tuple(best(1, ys, v)[0]/face(1, ys) for v in range(4, 8))
    assert ratios[0] == 1 and ratios[1] > 1 > ratios[2] and ratios[3] > 1
    print('Nonunimodal exact profile ratios at N=4,5,6,7:', *(str(x) for x in ratios))
    ys = (0,)*25+(3,)*12
    b = Q(sum(ys), len(ys))
    variance = sum((y-b)**2 for y in ys)/len(ys)
    assert len(ys)*(1+b-variance/b)/2 == -1
    assert reduced(1, ys, 2, 3) > face(1, ys)
    print('First-order coefficient -1 with strict finite witness (m,N)=(2,5),'
          ' black draws 25 zeros and 12 threes')


if __name__ == '__main__':
    exact_profiles()
    high_precision()
    joint_examples()
