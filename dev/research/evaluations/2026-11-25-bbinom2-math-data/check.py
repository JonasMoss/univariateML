"""Exact samplewise uniqueness certificates and symmetric-kernel checks."""
from fractions import Fraction as Q
from itertools import product

import mpmath as mp


def delta(counts):
    m = len(counts)-1
    n = sum(counts)
    s = sum(k*w for k,w in enumerate(counts))
    q = sum(k*k*w for k,w in enumerate(counts))
    return m*n*q - (m-1)*s*s - m*n*s


def certify(counts, max_slabs=4096):
    m = len(counts)-1
    n = sum(counts)
    interior = sum(counts[1:-1])
    assert m >= 2 and interior > 0 and delta(counts) > 0
    a = [sum(counts[j+1:]) for j in range(m)]
    b = [sum(counts[:m-j]) for j in range(m)]
    stop = n*sum(Q(1,j) for j in range(1,m))/interior

    def side(weights, p, s, power, order):
        return sum(w*j**order/(p+j*s)**power
                   for j,w in enumerate(weights) if w and (j or not order))

    def mean_bound(low, high, upper):
        left, right = Q(0), Q(1)
        for _ in range(48):
            p = (left+right)/2
            score = (side(a,p,low if upper else high,1,0)
                     - side(b,1-p,high if upper else low,1,0))
            if score > 0:
                left = p
            else:
                right = p
        return right if upper else left

    pending = [(Q(0), stop)]
    visits = excluded = decreasing = 0
    root_slabs = []
    while pending and visits < max_slabs:
        low, high = pending.pop()
        visits += 1
        lo = mean_bound(low,high,False)
        hi = mean_bound(low,high,True)
        if 0 < lo <= hi < 1:
            def bounds(power, order):
                xmin = side(a,hi,high,power,order)
                xmax = side(a,lo,low,power,order)
                ymin = side(b,1-lo,high,power,order)
                ymax = side(b,1-hi,low,power,order)
                return xmin,xmax,ymin,ymax

            def denom(s,power):
                return sum(n*j**power/(1+j*s)**power for j in range(1,m))

            xmin,xmax,ymin,ymax = bounds(1,1)
            if xmin+ymin > denom(low,1) or xmax+ymax < denom(high,1):
                excluded += 1
                continue
            xmin,_,ymin,_ = bounds(2,2)
            ss = denom(low,2)-xmin-ymin
            xmin,_,ymin,_ = bounds(2,0)
            mean_information = xmin+ymin
            xmin,xmax,ymin,ymax = bounds(2,1)
            mixed = max(abs(ymin-xmax),abs(ymax-xmin))
            if ss + mixed*mixed/mean_information < 0:
                decreasing += 1
                root_slabs.append((low,high))
                continue
        mid = (low+high)/2
        pending.extend(((mid,high),(low,mid)))

    return not pending, visits, excluded, decreasing, root_slabs


def symmetric_kernel(counts):
    m = len(counts)-1
    n = sum(counts)
    coefficients = [n]*m+[0]*(m-1)
    for k,w in enumerate(counts):
        d = abs(2*k-m)
        for j in range(2*d-1):
            coefficients[m-d+j] -= w*min(j+1,2*d-1-j)
    return coefficients


def symmetric_checks():
    mp.mp.dps = 80
    checked = 0
    for m in range(2,13):
        for half in product(range(3), repeat=m//2+1):
            counts = half + half[:(m+1)//2][::-1]
            if not (sum(counts[1:-1]) and delta(counts)>0):
                continue
            coeff = symmetric_kernel(counts)
            assert sum(coeff) < 0
            first_negative = next(j for j,v in enumerate(coeff) if v<0)
            assert all(v<=0 for v in coeff[first_negative:])
            assert next(v for v in coeff if v) > 0
            if checked % 23 == 0:
                for raw in ("0.00001","0.2","0.97","0.999999"):
                    z = mp.mpf(raw)
                    lhs = 2*sum(w*(1-z**(2*k)) for k,w in enumerate(counts))
                    lhs -= sum(counts)*(1+z)*(1-z**m)
                    rhs = (1-z)**2*mp.polyval(coeff[::-1],z)
                    assert abs(lhs-rhs) < mp.mpf("1e-73")
            checked += 1
    print("symmetric positive-Delta tables",checked,flush=True)


if __name__ == "__main__":
    symmetric_checks()
    examples = [
        (2,1,0,1),
        (1,1,0,0,2),
        (2,0,1,0,0,0,1),
        (5,0,0,1,0,2,0,0,0,0,1),
        (8000,12000,6000,1001),
    ]
    for counts in examples:
        ok, visits, excluded, decreasing, _ = certify(counts)
        assert ok
        print(counts,"Delta",delta(counts),"slabs",visits,excluded,decreasing,flush=True)
