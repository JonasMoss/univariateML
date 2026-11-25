"""Exact resultant upper bound on positive dispersion stationary points."""
import sympy as sp

from check import delta


def positive_resultant_roots(counts):
    p, s = sp.symbols("p s")
    m = len(counts) - 1
    n = sum(counts)
    a = [sum(counts[j+1:]) for j in range(m)]
    b = [sum(counts[:m-j]) for j in range(m)]
    A = sum(w/(p+j*s) for j,w in enumerate(a) if w)
    B = sum(w/(1-p+j*s) for j,w in enumerate(b) if w)
    G = (
        sum(j*w/(p+j*s) for j,w in enumerate(a) if j and w)
        + sum(j*w/(1-p+j*s) for j,w in enumerate(b) if j and w)
        - n*sum(j/(1+j*s) for j in range(1,m))
    )
    f, g = (sp.Poly(sp.cancel(score).as_numer_denom()[0], p, s)
            for score in (A-B,G))
    common = sp.gcd(f,g)
    resultant = f.exquo(common).resultant(g.exquo(common))
    poly = sp.Poly(resultant, s)
    assert not poly.is_zero
    while poly.eval(0) == 0:
        poly = poly.exquo(sp.Poly(s,s))
    return poly.sqf_part().count_roots(0, sp.oo)


if __name__ == "__main__":
    for counts in [(2,1,0,1), (5,0,2,0,1), (1,0,1,4)]:
        assert delta(counts) > 0
        candidates = positive_resultant_roots(counts)
        assert candidates >= 1
        print(counts, "positive resultant roots", candidates, flush=True)
