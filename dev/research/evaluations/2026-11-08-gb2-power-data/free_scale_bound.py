"""Numerical sanity check of the exact free-scale exterior proof, not an MLE oracle."""
import json
import mpmath as m
m.mp.dps = 80
logs = [m.log(m.mpf(x)) for x in ['.03', '.1', '.3', '.7', '1', '2', '5', '20']]
n = m.mpf(len(logs))
M, low = max(logs), min(logs)
S0, D = n*M-sum(logs), sum(logs)-n*low
k = sum(v == M for v in logs)
delta = S0/(4*n)
face = n*m.log(n/S0)-n-sum(logs)
softplus = lambda t: max(t, 0)+m.log1p(m.exp(-abs(t)))
rows = []
for text_q in ['.4', '1', '1.1', '2', '30']:
    q = m.mpf(text_q)
    C = max(0, m.digamma(q)-m.digamma(1))
    above = n*C/(S0*(-m.expm1(-q*k*softplus(-C)/n))) if C else 0
    near = max(4*n*C/S0, 4*n*n/(q*k*S0), 4*n*n*C/(q*k*S0*m.log(2)))
    K1 = n*q*m.log(q)-n*m.loggamma(q)-sum(logs)
    K2 = n*q*(m.log(n*q)-1+softplus(n))-n*m.loggamma(q)-sum(logs)
    A = 2*max(1, above, near, 2*n/(q*delta), 2*n/(q*D))
    for _ in range(100):
        if max(n*m.log(A)-q*A*delta+K1, n*m.log(A)-q*A*D+K2) < face-1:
            break
        A *= 2
    else:
        raise RuntimeError('Could not verify the analytic far-exterior threshold')
    assert A > above and A >= near
    gap = -m.inf
    count = 0
    for a in [A, 2*A, 10*A]:
        locations = [M+u/a for u in [0, C/2, C, C+1, 2*C+1, m.log(a), m.log(a)**2]]
        locations += [M-v/a for v in [0, C, 1, 2, a*delta/2, a*delta]]
        locations += [M-delta-v for v in [m.mpf('.1'), 1, 10]]+[low-1, low+1]
        shapes = [(n/S0)*r/a for r in [m.mpf('.01'), m.mpf(1)/3, 1, 3, 100]]+[m.mpf(1), q, m.mpf(1000)]
        normalizers = [n*m.log(a)-n*m.log(m.beta(p, q))-sum(logs) for p in shapes]
        for location in locations:
            sm = sum(softplus(-a*(v-location)) for v in logs)
            sp = sum(softplus(a*(v-location)) for v in logs)
            for p, normalizer in zip(shapes, normalizers):
                difference = normalizer-p*sm-q*sp-face
                assert difference < 0, (text_q, a, location, p, difference)
                gap = max(gap, difference)
                count += 1
    rows.append(dict(q=text_q, power_threshold=str(A), face_loglik=str(face), largest_sampled_gap=str(gap), sampled_points=count))
print(json.dumps(dict(role=__doc__, results=rows), indent=2))
