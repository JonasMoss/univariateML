"""Multiprecision spot checks of the finite cover's arithmetic."""
import json
from pathlib import Path

import mpmath as mp
import numpy as np

from cover import stirling_remainder, LD, digamma, height, kernel, trigamma


def mpval(x):
    return mp.mpf(str(x))


def relative_scaled(got, expected):
    return abs(mpval(got)-expected)/(1+abs(expected))


def main():
    mp.mp.dps = 85
    result = {}
    worst = [mp.mpf(0),mp.mpf(0)]
    count = 0
    for token in ("0","1e-35","1e-19","1e-12",".0001",".01",".49",".5",".91",".99999","1"):
        r = LD(token)
        num, den = r.as_integer_ratio()
        ratio = mp.mpf(num)/den
        for z in map(LD,("-8750","-1000","-80","-40","-39","-1","-1e-8","0",
                          "1e-8","1","39","40","80","1000","8750")):
            x, t = abs(mpval(z)), ratio if z >= 0 else 1-ratio
            if t == 0:
                d = mp.expm1(x)-x
            elif t == 1:
                d = x+mp.expm1(-x)
            else:
                d = (mp.log1p(t*mp.expm1(x))-t*x)/(t*(1-t))
            u = -mp.expm1(-x)
            g = mp.sign(z)*u/(t+(1-t)*mp.exp(-x))
            for j, (derived, expected) in enumerate(zip(
                    (kernel(np.array([z]), (r,r))[0],kernel(np.array([z]), (r,r),True)[0]),
                    (d,g))):
                worst[j] = max(worst[j],relative_scaled(derived,expected))
            count += 1
    assert max(worst) < mp.mpf("1e-13")
    result["kernel"] = dict(points=count, worst_scaled_error=worst)

    worst = [mp.mpf(0)]*4
    count = 0
    for x in (*[LD(10)**k for k in range(-8,16)], *map(LD,(".37",".85","15.9","16","16.1"))):
        t = mpval(x)
        R = mp.loggamma(t)-(t-mp.mpf(".5"))*mp.log(t)+t-mp.log(2*mp.pi)/2
        h = t*(mp.log(t)-mp.digamma(t))-mp.mpf(".5")
        for j,(derived,expected) in enumerate(zip(
                (*stirling_remainder(x),digamma(x),trigamma(x)),
                (R,h,mp.digamma(t),mp.polygamma(1,t)))):
            worst[j] = max(worst[j],relative_scaled(derived,expected))
        count += 1
    assert max(worst) < mp.mpf("1e-13")
    result["special_functions"] = dict(points=count,worst_scaled_error=worst)

    worst = [mp.mpf(0)]*2
    count = 0
    for k in map(LD,(".000001",".001",".3","1","100","10000")):
        for r in map(LD,("1e-35","1e-12",".003",".5",".997",".999999999999")):
            m, t = mpval(k), mpval(r)
            # Text conversion near 1 loses relative precision in the complement.
            # Use an exact dyadic reconstruction for these scalar inputs.
            num, den = r.as_integer_ratio()
            t = mp.mpf(num)/den
            p,q = m/(1-t),m/t
            s = p+q
            f = lambda x: x*(mp.log(x)-mp.digamma(x))
            normalizer = p*mp.log(t)+q*mp.log1p(-t)-mp.log(mp.beta(p,q))
            for j,(derived,expected) in enumerate(zip(
                    height(k,r),(normalizer,(f(p)+f(q)-f(s))/m))):
                worst[j] = max(worst[j],relative_scaled(derived,expected))
            count += 1
    assert max(worst) < mp.mpf("1e-13")
    result["height"] = dict(points=count,worst_scaled_error=worst)
    result["precision"] = mp.mp.dps
    Path(__file__).with_name("arithmetic.json").write_text(
        json.dumps(result,indent=2,default=lambda x:mp.nstr(x,12))+"\n")
    print("Arithmetic checks passed", flush=True)


if __name__ == "__main__":
    main()
