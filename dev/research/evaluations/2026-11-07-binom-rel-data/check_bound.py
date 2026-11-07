"""Independent 70-digit retained-kernel inequalities; no fitter code imported."""
import json
import mpmath as mp
mp.mp.dps = 70
checks = 0
min_slack = mp.inf
for L in (0, 1, 2):
    data = [max(L, 1), 2, 3, 4]
    for K in (4, 7, 12):
        for N in (K, 2*K, 100*K):
            c = lambda k: mp.fsum(mp.log1p(-mp.mpf(j)/N) for j in range(k))
            D = mp.fsum(c(v)-c(K) for v in data)
            for rate in (mp.mpf('.1'), mp.mpf('2'), mp.mpf('15')):
                w = lambda k: rate**k/mp.factorial(k)
                B = mp.fsum(w(k)*mp.exp(c(k)) for k in range(L, N+1))
                Z = mp.fsum(w(k) for k in range(L, K+1))
                ell = mp.fsum(mp.log(w(v))+c(v)-mp.log(B) for v in data)
                retained = mp.fsum(mp.log(w(v))-mp.log(Z) for v in data)
                slack = retained + D - ell
                assert slack >= -mp.mpf('1e-60')
                min_slack = min(min_slack, slack)
                checks += 1
# Inflated zeros use the same correction, including held inflation.
data = [0, 0, 1, 2, 3, 4]
for K in (4, 7, 12):
    for N in (K, 2*K, 100*K):
        c = lambda k: mp.fsum(mp.log1p(-mp.mpf(j)/N) for j in range(k))
        D = mp.fsum(c(v)-c(K) for v in data)
        for rate in (mp.mpf('.1'), mp.mpf('2'), mp.mpf('15')):
            w = lambda k: rate**k/mp.factorial(k)
            B = mp.fsum(w(k)*mp.exp(c(k)) for k in range(N+1))
            Z = mp.fsum(w(k) for k in range(K+1))
            for pi in (mp.mpf(0), mp.mpf('.2'), mp.mpf('.9')):
                ell = mp.fsum(mp.log(pi+(1-pi)/B) if v == 0 else mp.log((1-pi)*w(v)*mp.exp(c(v))/B) for v in data)
                retained = mp.fsum(mp.log(pi+(1-pi)/Z) if v == 0 else mp.log((1-pi)*w(v)/Z) for v in data)
                slack = retained + D - ell
                assert slack >= -mp.mpf('1e-60')
                min_slack = min(min_slack, slack)
                checks += 1
print(json.dumps({'digits': mp.mp.dps, 'inequalities': checks, 'minimum_slack': str(min_slack)}))
