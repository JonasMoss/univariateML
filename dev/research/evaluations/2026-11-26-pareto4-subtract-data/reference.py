"""Reference likelihood on exact and binary64-rounded observations."""
import mpmath as mp

mp.mp.dps = 100
U = 2**54
for sample in ((-5, 10), (5, 9), (1, 5), (-1, 1)):
    for rounded in (False, True):
        offsets = [int(float(U + x)) - U if rounded else x for x in sample]
        logs = [mp.log1p(mp.mpf(x) / U) for x in offsets]
        print(sample, "rounded" if rounded else "exact", offsets)
        if min(logs) >= 0 and sum(logs) > 0:
            print("held-scale face a", mp.nstr(len(logs) / sum(logs), 65))
        if min(logs) < 0:
            def profile(q):
                t = [q * U * w for w in logs]
                a = len(t) / sum(mp.log1p(mp.exp(v)) for v in t)
                return len(t) * mp.log(a*q) - len(t) - sum(mp.log1p(mp.exp(-v)) for v in t)

            q = mp.findroot(lambda q: mp.diff(profile, q), (.1, .4))
            a = len(logs) / sum(mp.log1p(mp.exp(q*U*w)) for w in logs)
            print("held-scale power/U", mp.nstr(q, 65), "shape1", mp.nstr(a, 65))
        q = mp.mpf(1)/4
        a = len(logs) / sum(mp.log1p(mp.exp(q*U*w)) for w in logs)
        print("held-scale and power shape1", mp.nstr(a, 65))
        if logs[0] != logs[1]:
            print("joint Pareto endpoint", U + min(offsets), "a", mp.nstr(2/abs(logs[1]-logs[0]), 65))
        powered = [mp.exp(q*U*(w-max(logs))) for w in logs]
        mean = mp.fsum(powered)/len(powered)
        print("held power, Weibull scale offset", mp.nstr(U*mp.expm1(max(logs)+mp.log(mean)/(q*U)), 65))
        if sample == (-5, 10):
            def gap(z):
                rate = mp.exp(z)
                g = mp.fsum(mp.log1p(rate*u) for u in powered)/len(powered)
                return mp.log(rate*mean/g)-g

            if not rounded:
                score = lambda z: mp.diff(gap, z)
                brackets = [(z, z+1) for z in range(-8, 12) if score(z) > 0 > score(z+1)]
                assert len(brackets) == 1
                rate_log = mp.findroot(score, brackets[0], solver="anderson")
                assert gap(rate_log) > 0
                rate = mp.exp(rate_log)
                shape = len(powered)/mp.fsum(mp.log1p(rate*v) for v in powered)
                offset = U*mp.expm1(max(logs)-rate_log/(q*U))
                print("held power, finite shape1", mp.nstr(shape, 65), "scale offset", mp.nstr(offset, 65))
                print("held power, finite gain per point", mp.nstr(gap(rate_log), 65))
