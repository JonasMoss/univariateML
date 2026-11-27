"""High-precision spot checks of the inequalities and boundary traces."""
import json

import mpmath as mp


mp.mp.dps = 80
D = mp.mpf
grid = list(map(D, ('0.01', '0.1', '0.5', '1', '2', '10', '100', '10000')))
count = 0
minimum_margin = mp.inf
for p in grid:
    for q in grid:
        contrast = mp.digamma(p + q) - mp.digamma(q)
        information = mp.polygamma(1, q) - mp.polygamma(1, p + q)
        margin = q * information + mp.expm1(-contrast)
        assert margin > 0
        minimum_margin = min(minimum_margin, margin)
        count += 1
        if q > 1:
            p_information = mp.polygamma(1, p) - mp.polygamma(1, p + q)
            assert p_information > (q - 1) / (p * (p + q - 1))

distances = tuple(map(D, ('0', '0', '0.25', '2')))
mean = lambda xs: mp.fsum(xs) / len(xs)
softplus = lambda z: max(z, 0) + mp.log1p(mp.exp(-abs(z)))
rate = 1 / mean(distances)


def genbeta(p, q, a, shift):
    ys = [d + shift for d in distances]
    return (mp.log(a) - mp.log(mp.beta(p, q)) - p * a * mean(ys)
            + (q - 1) * mean([mp.log(-mp.expm1(-a * y)) for y in ys]))


def gb2(p, q, a, shift):
    zs = [a * (d + shift) for d in distances]
    return (mp.log(a) - mp.log(mp.beta(p, q))
            - mean([p * softplus(-z) + q * softplus(z) for z in zs]))


power_value = mp.log(rate) - 1
traces = []
for a in (D(10)**3, D(10)**6, D(10)**9):
    contact = D(2)
    finite_shift = contact / a
    full_shift = mp.log(a) / a
    q = D('2.5')
    gb1_penalty = (q - 1) * D('0.5') * mp.log(-mp.expm1(-contact))
    gb2_penalty = -q * D('0.5') * softplus(-contact)
    errors = {
        'genbeta_finite_contact': genbeta(rate/a, q, a, finite_shift)
        - power_value - gb1_penalty,
        'genbeta_full_contact': genbeta(rate/a, q, a, full_shift) - power_value,
        'gb2_finite_contact': gb2(q, rate/a, a, finite_shift)
        - power_value - gb2_penalty,
        'gb2_full_contact': gb2(q, rate/a, a, full_shift) - power_value,
    }
    assert all(abs(value) < 20 * mp.log(a) / a for value in errors.values())
    traces.append({'power': int(a), **{key: mp.nstr(value, 12)
                                     for key, value in errors.items()}})

for reverse_shape in (D(1000), D(10)**6, D(10)**9):
    p, a = D('2.5'), D('0.75')
    # The mode is zero when the logistic intercept is log(q/p).
    observed = gb2(p, reverse_shape, a, mp.log(p/reverse_shape)/a)
    expected = (mp.log(a) + p*mp.log(p) - mp.loggamma(p)
                + p*a*mean(distances)
                - p*mean([mp.exp(a*d) for d in distances]))
    assert abs(observed - expected) < 100 / reverse_shape

print(json.dumps({'decimal_digits': mp.mp.dps, 'shape_pairs': count,
                  'minimum_curvature_margin': mp.nstr(minimum_margin, 12),
                  'contact_traces': traces, 'gamma_face_checks': 3}, indent=2))
