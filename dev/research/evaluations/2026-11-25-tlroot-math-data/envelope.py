"""Replay the two n=31 box probes, with and without a shared-scale bound."""
import heapq
import itertools
import json
import math
from pathlib import Path
from time import perf_counter

import mpmath as mp

LOG2 = math.log(2)
PAD = 2e-10
CAP = 127999
N = 31
PRIOR = Path(__file__).resolve().parent.parent / "2026-11-07-tlambda-impl-data"


def data():
    mp.mp.dps = 70
    shape = .14
    x = []
    max_error = mp.mpf(0)
    for i in range(N):
        p = (i+.5)/N
        left = p < .5
        if p == .5:
            x.append(0.)
            continue
        lp = math.log(p) if left else math.log1p(-p)
        lq = math.log1p(-math.exp(lp))
        log_mag = shape*lq+math.log(-math.expm1(-shape*(lq-lp)))-math.log(shape)
        value = math.copysign(math.exp(log_mag),-1 if left else 1)
        x.append(value)
        # Audit the port of qtlambda at the recorded binary64 inputs.
        p_mp, l_mp = mp.mpf(p), mp.mpf(shape)
        exact = (p_mp**l_mp-(1-p_mp)**l_mp)/l_mp
        max_error = max(max_error,abs(exact-value))
    assert max_error < mp.mpf("2e-14")
    print(f"largest binary64 sample error versus 70 digits {mp.nstr(max_error,8)}")
    return x

def distances(x, a, b):
    return [max(a-z,z-b,0.) for z in x]


def shared(d, lo, hi, upper_shape, lower_shape):
    logm = (upper_shape-2)*LOG2
    A = min(1.,max(.5,-lower_shape))
    beta = 1+1/A
    logc = -beta*math.log(A)
    shifted = [math.log(r)+(logm-logc)/beta for r in d if r]
    needed = math.ceil(N/beta)
    s = lo if len(shifted)<needed else min(hi,max(lo,sorted(shifted)[-needed]))
    return N*(logm-s)+beta*sum(min(0,s-k) for k in shifted)+PAD


def logp_upper(z, l):
    """Upper bracket endpoint for smaller-tail probability, in log coordinates."""
    if z == 0:
        return -LOG2
    if l == -1.:
        return LOG2-math.log(z+2+math.hypot(z,2))
    if l == 0.:
        return -z-math.log1p(math.exp(-z))
    if l < 0.:
        a = -l
        left = -math.log(a*z+2**a)/a
        right = min(-LOG2,-math.log1p(a*z)/a)
        # Even where powers nearly cancel, evaluate their difference with expm1.
        for _ in range(29):
            mid = (left+right)/2
            qlog = math.log1p(-math.exp(mid))
            f = math.exp(-a*qlog)*math.expm1(a*(qlog-mid))/a-z
            if f > 0:
                left = mid
            else:
                right = mid
        return right
    deficit = math.fma(-l,z,1.)
    if deficit <= 0.:
        return -math.inf
    left = (math.log(deficit)-LOG2)/l
    right = min(-LOG2,math.log(deficit)/l)
    for _ in range(29):
        mid = (left+right)/2
        qlog = math.log1p(-math.exp(mid))
        f = math.exp(l*mid)-math.expm1(l*qlog)-deficit
        if f < 0:
            left = mid
        else:
            right = mid
    return right


def audit_inverse():
    pairs = []
    for shape in (-1.,-.875,-.5,-.125,-.0078125,0.,.0078125,.125,.25,.5,.9375):
        zs = [1e-8,.01,.2,.7,1.,3.,10.,100.]
        if shape > 0:
            zs += [(1-h)/shape for h in (1e-2,1e-6,1e-10)]
        pairs.extend((shape,z) for z in zs if shape <= 0 or z*shape < 1)
    worst = mp.mpf(0)
    for q,z in pairs:
        shape, x = mp.mpf(q), mp.mpf(z)
        if shape == 0:
            expected = -mp.log1p(mp.exp(x))
        else:
            a = -shape
            if shape < 0:
                left = -mp.log(a*x+2**a)/a
                right = min(-mp.log(2),-mp.log1p(a*x)/a)
            else:
                deficit = 1-shape*x
                left = (mp.log(deficit)-mp.log(2))/shape
                right = min(-mp.log(2),mp.log(deficit)/shape)
            for _ in range(220):
                mid = (left+right)/2
                p = mp.exp(mid)
                v = (mp.exp(shape*mp.log1p(-p))-p**shape)/shape
                if v > x:
                    left = mid
                else:
                    right = mid
            expected = (left+right)/2
        estimate = mp.mpf(logp_upper(z,q))
        if q > 0:
            # Binary64 inversion of the support deficit is less accurate
            # close to contact. The benchmark handles that deficit below.
            deficit_error = abs(mp.mpf(math.fma(-q,z,1.))-(1-shape*x))/(1-shape*x)/shape
        else:
            deficit_error = mp.mpf(0)
        worst = max(worst, expected-estimate-deficit_error)
        assert estimate >= expected-deficit_error-mp.mpf("2e-12"), (q,z,estimate,expected,deficit_error)
    print(f"70-digit inverse audit {len(pairs)} pairs, largest adjusted "
          f"shortfall {mp.nstr(worst,6)}")

def inverse_upper(d, lo, hi, lower_shape, upper_shape):
    if lower_shape > 0 and max(d) > math.exp(hi)/lower_shape*(1+1e-14):
        return -math.inf
    if upper_shape == 1:
        return -N*(lo+LOG2)+PAD
    scale = math.exp(hi)
    if lower_shape == upper_shape == -1:
        hs = [math.hypot(2,r/scale) for r in d]
        return -N*lo-sum(math.log(h)+math.log(h+2) for h in hs)+PAD
    b = 1-upper_shape
    total = -N*lo
    for r in d:
        lp = logp_upper(r/scale,lower_shape)
        if lp == -math.inf:
            # A rounded equality at a support edge gets a conservative bound.
            if r*lower_shape >= scale*(1-1e-14):
                total -= (2-upper_shape)*LOG2
                continue
            raise ArithmeticError("unexpected tail at endpoint")
        total += b*lp-math.log1p(math.exp(b*(lp-math.log1p(-math.exp(lp)))))
    return total+PAD


def negative_eval(d,s):
    hs = [math.hypot(2,r*math.exp(-s)) for r in d]
    value = -N*s-sum(math.log(h)+math.log(h+2) for h in hs)
    score = N-sum(2/h+4/h**2 for h in hs)
    return value,score


def negative_scale(d,a,b):
    va,sa = negative_eval(d,a)
    vb,sb = negative_eval(d,b)
    if sa <= 0:
        return va+PAD
    if sb >= 0:
        return vb+PAD
    for _ in range(30):
        m = (a+b)/2
        value,score = negative_eval(d,m)
        if score < 0:
            b = m
        else:
            a = m
    return value+max(score*(a-m),score*(b-m))+PAD


def search(x, lower, upper, incumbent, kind):
    goal = incumbent+1e-7*N
    start = perf_counter()
    order = itertools.count()
    heap = []
    inversions = cheap_prunes = count = 0
    width = [b-a for a,b in zip(lower,upper)]

    def push(a,b):
        nonlocal inversions, cheap_prunes, count
        count += 1
        d = distances(x,a[0],b[0])
        v = math.inf
        if kind != "inverse":
            v = shared(d,a[1],b[1],b[2],a[2])
            if v <= goal:
                cheap_prunes += 1
                return
        if kind in ("inverse","cascade"):
            inversions += 1
            v = min(v,inverse_upper(d,a[1],b[1],a[2],b[2]))
        if kind == "negative profiled":
            v = min(v,negative_scale(d,a[1],b[1]))
        if v > goal:
            heapq.heappush(heap,(-v,next(order),a,b))

    push(lower,upper)
    stop = "gap"
    while heap and count+2 <= CAP:
        item = heapq.heappop(heap)
        _,_,a,b = item
        dims = range(1 if kind == "negative profiled" else 3)
        midpoints = [a[j]+(b[j]-a[j])/2 for j in dims]
        eligible = [j for j in dims if a[j] < midpoints[j] < b[j]]
        if not eligible:
            heapq.heappush(heap,item)
            stop = "precision"
            break
        dim = max(eligible,key=lambda j: (b[j]-a[j])/width[j])
        mid = midpoints[dim]
        left,right = list(b),list(a)
        left[dim] = right[dim] = mid
        push(a,left)
        push(right,b)
    if heap and count == CAP:
        stop = "budget"
    gap = -heap[0][0]-incumbent if heap else 1e-7*N
    print(f"{kind:17} {count:7} {len(heap):7} {inversions:7} "
          f"{cheap_prunes:7} {gap:.10g} {perf_counter()-start:.3f}s {stop}",flush=True)
    expected = {"shared": {"precision", "budget"}, "negative profiled": {"gap"}}
    assert stop in expected.get(kind, {"budget"})
    return gap


def high_precision_points(x, fixed, incumbent):
    xs = list(map(mp.mpf,x))  # exact lift of the benchmark binary64 inputs
    if fixed:
        def value(s):
            hs = [mp.sqrt(4+(z*mp.exp(-s))**2) for z in xs]
            return -N*s-sum(mp.log(h)+mp.log(h+2) for h in hs)
        s = mp.findroot(lambda s: mp.diff(value,s), -1)
        point = (0,float(s),-1)
        result = value(s)
    else:
        def value(s,l):
            if l <= 0:
                raise ValueError("expected positive local solution")
            total = -N*s
            for z in xs:
                d = 1-l*abs(z)*mp.exp(-s)
                p = mp.findroot(lambda p: p**l-mp.expm1(l*mp.log1p(-p))-d,
                                ((d/2)**(1/l),min(mp.mpf('.5'),d**(1/l))),
                                solver='anderson') if z else mp.mpf('.5')
                total -= mp.log(p**(l-1)+(1-p)**(l-1))
            return total
        # Implicit inverse bisection in findroot is unsuitable for mp.diff's
        # extra precision. Audit a small local proposal grid instead.
        best = (-math.inf,0,0)
        for shape in [i/100 for i in range(5,31)]:
            for s in [i/100 for i in range(-18,8)]:
                if shape*max(xs)*math.exp(-s) >= 1:
                    continue
                val = -N*s+sum((1-shape)*lp-math.log1p(
                    math.exp((1-shape)*(lp-math.log1p(-math.exp(lp)))))
                    for z in x for lp in [logp_upper(abs(z)*math.exp(-s),shape)])
                best = max(best,(val,s,shape))
        _,s,l = best
        point = (0,s,l)
        result = value(mp.mpf(s),mp.mpf(l))
    print(f"point {point}, 70-digit log likelihood {mp.nstr(result,23)}, "
          f"shared bound {shared(list(map(abs,x)),point[1],point[1],point[2],point[2]):.12f}")
    if fixed:
        assert abs(result-incumbent) < mp.mpf("1e-10")
    assert shared(list(map(abs,x)),point[1],point[1],point[2],point[2]) > incumbent+1e-7*N


if __name__ == '__main__':
    start = perf_counter()
    x = data()
    audit_inverse()
    for fixed,name in [(True,"negative-box-probe.json"),(False,"box-probe.json")]:
        incumbent = json.loads((PRIOR/name).read_text())["incumbent"]
        target = incumbent-1e-7*N
        logm = (-3 if fixed else -1)*LOG2
        delta = min(b-a for a,b in zip(x,x[1:]))/3
        smin = min((target+2*N*math.log(delta))/N,
                   (target-logm+2*(N-1)*math.log(delta))/(N-2))
        radius = math.exp(logm/2-target/N)
        lower = [x[0]-radius,smin,-1]
        upper = [x[-1]+radius,logm-target/N,-1 if fixed else 1]
        print(f"fixed negative {fixed}, incumbent {incumbent:.16g}, "
              f"lower {lower}, upper {upper}",flush=True)
        high_precision_points(x,fixed,incumbent)
        print("method            boxes   pending inverse cheap_prune gap wall",flush=True)
        search(x,lower,upper,incumbent,"shared")
        original = search(x,lower,upper,incumbent,"inverse")
        historical_gap = json.loads((PRIOR/name).read_text())["global_upper"]-incumbent
        print(f"historical C++ gap {historical_gap:.10g}, Python difference {original-historical_gap:+.10g}",flush=True)
        assert abs(original/historical_gap-1) < .15
        search(x,lower,upper,incumbent,"cascade")
        if fixed:
            search(x,lower,upper,incumbent,"negative profiled")
            # Direct uniform-depth upper bound for the profiled method.
            coeff = N/(2*math.exp(smin))
            depth = math.ceil(math.log2(coeff*(upper[0]-lower[0])/(1e-7*N-PAD)))
            print(f"uniform depth bound {depth}, at most {2**(depth+1)-1} boxes "
                  "if the incumbent is the global optimum",flush=True)
    print(f"All envelope checks passed in {perf_counter()-start:.3f}s")
