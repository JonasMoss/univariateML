"""Construct two equal finite U maxima, using exact distances and intervals."""
from fractions import Fraction
import math
from pathlib import Path
import sys

import mpmath as mp

sys.path.insert(0, str(Path(__file__).resolve().parent.parent
                       / "2026-11-25-tlroot-math-data"))
from roots import box, describe, hi, inverse, lo, Profile, rat, sign
from scenarios import terms

mp.mp.dps = 205
mp.iv.dps = 185
N, CENTERS, MODERATE = 1471, 350, 1100
EXTREME = 1-Fraction(1, 2**512)


def profile(t):
    return Profile(N, CENTERS, [(t, MODERATE), (rat(EXTREME), 1)])


def point(t, shape):
    r = mp.mpf(EXTREME.numerator)/EXTREME.denominator
    a, h = terms(t,shape)
    b, h_r = terms(r,shape)
    return (-N/shape+MODERATE*a+b+CENTERS*mp.log(2),
            -N*mp.log(shape)+MODERATE*h+h_r+CENTERS*(shape-1)*mp.log(2))


def system(a,b,t):
    s_a, h_a = point(t,a)
    s_b, h_b = point(t,b)
    return s_a,s_b,h_b-h_a


def height_derivative(t, collar):
    l = box(*collar)
    p = inverse(t,l)
    q = 1-p
    d = p**(l-1)+q**(l-1)
    return MODERATE*(l-1)*(p**(l-2)-q**(l-2))/(l*d**2)


def max_height(P, collar):
    a,b = P.narrow(*collar,1,-1,mp.mpf("1e-20"))
    assert b-a < mp.mpf("1e-19")
    return P.local(a,b)[0]


def main():
    print(f"mpmath {mp.__version__}, interval digits {mp.iv.dps}",flush=True)
    early, late, t = mp.findroot(system,tuple(map(mp.mpf,("1.010", "1.602", ".7513"))),
                                 tol=mp.mpf("1e-75"))
    print("Numerical scout",*(mp.nstr(v,50) for v in (early,late,t)),flush=True)
    den = 10**28
    floor = int(mp.floor(den*t))
    assert floor == 7511416680063824861674766027
    endpoints = (Fraction(floor,den),Fraction(floor+1,den))
    print("t endpoints",*(str(v) for v in endpoints),flush=True)
    # Translate the upper endpoint to zero. The exceptional observation
    # becomes -2^-512, exactly representable in binary64.
    low_x = float(endpoints[0]-1)
    if Fraction(low_x) >= endpoints[0]-1:
        low_x = math.nextafter(low_x,-math.inf)
    high_x = math.nextafter(low_x,math.inf)
    neighbors = (1+Fraction(low_x),1+Fraction(high_x))
    assert neighbors[0] < endpoints[0] < endpoints[1] < neighbors[1]
    T = box(lo(rat(neighbors[0])),hi(rat(neighbors[1])))
    P = profile(T)
    scale = 2**20
    aa,bb = (int(mp.floor(v*scale)) for v in (early,late))
    pairs = [(j-16,j+17) for j in (aa,bb)]
    assert pairs == [(1058972,1059005),(1667557,1667590)]
    collars = [(mp.mpf(a)/scale,mp.mpf(b)/scale) for a,b in pairs]
    print("shape collars",pairs,"over",scale,flush=True)
    for a,b in collars:
        assert sign(P.eval(a,a)[1]) == 1 and sign(P.eval(b,b)[1]) == -1
        value, _, curvature, _, _ = P.local(a,b)
        assert sign(curvature) == -1 and sign(value) == 1
        root = P.narrow(a,b,1,-1,mp.mpf("1e-13"))
        assert root[1]-root[0] < mp.mpf("1e-12")
        print("  positive collar heights",mp.nstr(lo(value),15),mp.nstr(hi(value),15),
              "negative score slopes",mp.nstr(lo(curvature),12),mp.nstr(hi(curvature),12),
              "root bracket",*(mp.nstr(t,17) for t in root),flush=True)
    derivative = height_derivative(T,collars[1])-height_derivative(T,collars[0])
    assert sign(derivative) == 1
    print("late-minus-early derivative",mp.nstr(lo(derivative),15),
          mp.nstr(hi(derivative),15),flush=True)
    for t_expected, s in zip(endpoints,(-1,1)):
        fixed = profile(rat(t_expected))
        gap = max_height(fixed,collars[1])-max_height(fixed,collars[0])
        assert sign(gap) == s
        print("late-minus-early gap at endpoint",mp.nstr(lo(gap),15),
              mp.nstr(hi(gap),15),flush=True)
    for x, t_exact, s in zip((low_x,high_x),neighbors,(-1,1)):
        fixed = profile(rat(t_exact))
        gap = max_height(fixed,collars[1])-max_height(fixed,collars[0])
        assert sign(gap) == s
        print("binary64 x",x.hex(),"late-minus-early gap",mp.nstr(lo(gap),15),
              mp.nstr(hi(gap),15),flush=True)
    other = []
    for a,b in ((mp.mpf(1),collars[0][0]),(collars[0][1],collars[1][0]),
                (collars[1][1],mp.mpf(2))):
        roots,pending = P.isolate(a,b,goal=mp.mpf("1e-16"))
        assert not pending
        other.extend(roots)
    assert len(other) == 1
    assert sign(P.local(*other[0])[2]) == 1  # only other stationary point is a minimum
    describe(P,other,[])
    print("All exact finite-tie witness interval checks passed.")


if __name__ == "__main__":
    main()
