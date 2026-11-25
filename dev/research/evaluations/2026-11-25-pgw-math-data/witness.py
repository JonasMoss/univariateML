"""Certify a finite likelihood witness above both PGW face heights."""
import mpmath as mp

from check import conditional, threshold


def verify():
    iv = mp.iv
    sample = (1, 2, 4, 4, 4, 8)
    n = len(sample)
    x = [iv.mpf(i) for i in sample]
    y = [iv.ln(xi) for xi in x]
    span = y[-1] - y[0]
    ys = [(yi - y[0]) / span for yi in y]
    center = [mp.log(mp.mpf(i) / sample[0]) /
              mp.log(mp.mpf(sample[-1]) / sample[0])
              for i in sample]
    # Feasible Weibull (a,b,c) = (13/3, 11/6, 1) in normalized log units.
    wb = iv.mpf(11) / 6
    wa = iv.mpf(13) / 3
    z = [wb * (yi - iv.ln(wa)) for yi in y]
    witness = n * iv.ln(span * wb) + sum(zi - iv.exp(zi) for zi in z)

    b0, v0 = conditional(center, 0)
    b = iv.mpf(mp.nstr(b0, 22))
    v = iv.mpf(mp.nstr(v0, 22))
    u = [iv.exp(v + b * yi) for yi in ys]
    g = n * iv.ln(b) + sum(iv.ln(ui) + ui + 1 - iv.exp(ui) for ui in u)
    q = [1 - ui * (iv.exp(ui) - 1) for ui in u]
    gb = n / b + sum(yi * qi for yi, qi in zip(ys, q))
    gv = sum(q)
    # On a G superlevel of the witness, the normalized slope and u_max
    # follow from the exp(-u^2/2) density bound. See report for proof.
    B = 20 * n
    U = 10 * n
    assert B > n and n * iv.ln(B) - B < witness.a
    assert n * iv.ln(B) + iv.ln(U) - U**2 / 2 < witness.a
    vlo = witness - n * iv.ln(B)
    vhi = iv.ln(U)
    g_upper = g + abs(gb) * (B + abs(b))
    g_upper += abs(gv) * (abs(vlo) + abs(vhi) + abs(v))

    k0, _ = threshold(center)
    k = iv.mpf(mp.nstr(k0, 22))
    kd = [k * yi for yi in ys]
    t = n * iv.ln(k) + sum(ui + 1 - iv.exp(ui) for ui in kd)
    tk = n / k - sum(yi * (iv.exp(ui) - 1) for yi, ui in zip(ys, kd))
    assert n * iv.ln(B) - B**2 / 2 < witness.a
    t_upper = t + abs(tk) * (B + abs(k))
    print(n, 'G upper minus witness', g_upper - witness,
          'T upper minus witness', t_upper - witness, flush=True)
    return g_upper.b < witness.a and t_upper.b < witness.a


if __name__ == '__main__':
    mp.mp.dps = 85
    mp.iv.dps = 75
    assert verify()
