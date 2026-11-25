"""Outward comparisons for two all-free Burr XII transitions."""
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "2026-11-25-burr-math-data"))
from check import M, IV, mp, box, fixed, terms, weibull

iv = mp.iv
weights = [IV(1)/7]*7
numerical_weights = [M(1)/7]*7


def midpoint(v):
    return (M(v.a)+M(v.b))/2


def avg(x):
    return sum(w*v for w,v in zip(weights,x))


def pareto(y):
    return -1-iv.log(avg(y))


def pareto_family(q):
    return [IV(0), IV(23826)/100000*q, 7*q/15, 8*q/15, 8*q/15, IV(1), IV(1)]


def weibull_family(t):
    return [IV(0), 1-10*t/15, 1-8*t/15, 1-7*t/15, 1-7*t/15, IV(1), IV(1)]


def weibull_bracket(y, radius):
    shifted = [v-1 for v in y]
    def at(c):
        e = [iv.exp(c*v) for v in shifted]
        total = avg(e)
        score = 1/c+avg(shifted)-avg([t*v for t,v in zip(e,shifted)])/total
        value = iv.log(c)+c*avg(shifted)-iv.log(total)-1
        return score,value

    center = weibull(list(map(midpoint,y)), numerical_weights)[0]
    lo, hi = IV(center)-radius, IV(center)+radius
    gl, _ = at(lo)
    gh, _ = at(hi)
    assert gl.a > 0 and gh.b < 0
    enclosure = box(M(lo.a), M(hi.b))
    score, lower = at(IV(center))
    v = [1/avg([IV(1) if i == j else iv.exp(enclosure*(yi-yj))
                for i,yi in enumerate(y)])
         for j,yj in enumerate(y)]
    # At a fixed c evaluate W(c), then bound the value increase by a tangent.
    upper = lower+abs(score)*radius
    return lower,upper,enclosure,v


def cover(y, start, stop, limit):
    """Strictly upper-bound the whole compact profile by the given level."""
    data = list(map(midpoint,y))
    cache = {}
    def upper(k):
        if k not in cache:
            guess = None
            if cache:
                j = min(cache, key=lambda j: abs(float(j/k)-1))
                c, b = cache[j][1]
                guess = c, b+mp.log(j/k)
            try:
                c,b = fixed(data,numerical_weights,k,guess)
            except (ValueError, ZeroDivisionError):
                c,b = fixed(data,numerical_weights,k)
            p = [IV(1/(1+mp.exp(-c*t-b))) for t in data]
            j = max(range(len(p)), key=lambda j: float((p[j]*(1-p[j])).a))
            p[j] = (1/(1+IV(k))-sum(w*v for i,(w,v) in enumerate(zip(weights,p)) if i!=j))/weights[j]
            assert all(v.a>0 and v.b<1 for v in p)
            a = avg([((1+IV(k))*v-1)*t for v,t in zip(p,y)])
            assert a.a > 0
            entropy = -avg([v*iv.log(v)+(1-v)*iv.log(1-v) for v in p])
            value = iv.log(IV(k)/a)-1-(1+IV(k))*entropy
            cache[k] = value.b, (c,b)
            assert len(cache)<10000
        return cache[k][0]

    stack = []
    while start < stop:
        right = min(2*start, stop)
        stack.append((start,right))
        start = right
    accepted = 0
    worst = None
    while stack:
        lo,hi = stack.pop()
        bound = max(upper(lo),upper(hi))+(IV(hi)-lo)**2/(8*IV(lo)**2)
        if bound.b < limit:
            accepted += 1
            gap = IV(limit)-bound
            worst = gap.a if worst is None else min(worst,gap.a)
        else:
            mid = (lo+hi)/2
            assert (hi-lo)/lo > M('1e-9'), (lo,hi,bound,limit)
            stack.extend([(lo,mid),(mid,hi)])
    print("compact cover",len(cache),accepted,"smallest outward gap",float(worst),flush=True)


def two_finite():
    d = IV(1)/8
    k = IV(2)**-40
    repetitions = 2**90
    w = IV(1)/(7*repetitions+1)
    y = pareto_family(IV([0,1]))
    mean = avg(y)
    alpha = 1/mean
    b = iv.log((1-6*k)/(7*k))
    c = alpha/k
    q = k*iv.log(7*k/(1+k))+(IV(1)/7-6*k/7)*iv.log((1-6*k)/(1+k))
    error = (1+k)*6*k/(1-6*k)
    # The outlier logit is negative for the entire deformation.
    assert (b-IV("0.9")*c).b < 0
    outlier_lower = iv.log(alpha)+b-IV("1.1")*c-(1+k)
    pbulk = pareto(y)
    # Evaluate the differences algebraically to avoid interval dependency in P.
    witness_gap = (1-w)*(q-error)+w*(outlier_lower-pbulk)
    assert witness_gap.a > -IV('1e-8')
    p_drop = iv.log(((1-w)*mean+IV("0.9"))/mean)
    assert p_drop.a > IV('0.4')
    # At q=1 the prior result proves the minimum bulk P-W gap on the path.
    y1 = pareto_family(IV(1))
    _,wu,_,_ = weibull_bracket(y1,IV('1e-12'))
    assert (pareto(y1)-wu).a > IV('0.000002')
    assert (-iv.log(1-w)).b < IV('1e-10')
    mix_error = w*(iv.log(IV(1000))+2)
    assert mix_error.b < IV('1e-10')
    # A minimum weight of 1/7 gives the loosest separator entropy bound.
    a = d
    m = (1+d)/7
    entropy_deficit = a*iv.log(a/m)+(m-a)*iv.log((m-a)/m)
    assert entropy_deficit.b < -IV('0.05')
    k0 = IV(2)**-20
    m0 = (1+k0)/7
    q0 = k0*iv.log(k0/m0)+(m0-k0)*iv.log((m0-k0)/m0)
    assert q0.b < -IV("1e-6")
    K_uniform = 2**24
    assert (7*IV(K_uniform+1)/(2*IV(K_uniform+1-7)**2)).b < IV("1e-6")
    cmin = iv.exp(-IV("1.9"))/k0
    assert (cmin*(1-2*k0)/3).a > 8000
    assert ((1+k0)*iv.exp(-IV("0.9")*cmin)).b < (w/4).a
    assert (2*IV(2)**-120*1000*(1-w)/w).b < iv.log(IV(2)).a
    assert (2*IV(2)**-120*8000*w*(1+K_uniform)).b < IV("1e-9")
    # Envelope with the bulk endpoints, valid at every parameter triple.
    assert (iv.log(IV(1000))+1-1000*(1-w)/14).b < -2
    assert (1/IV(1000)-(1-w)/14).b < 0
    assert pbulk.a > -IV('1.89')
    # At the left endpoint exclude every k >= d using bulk bounds.
    y0 = pareto_family(IV(0))
    _,wu,_,_ = weibull_bracket(y0,IV('1e-12'))
    threshold = (pareto(y0)-IV('0.001')).a
    K = 64
    assert (wu+7*IV(K+1)/(2*IV(K+1-7)**2)).b < threshold
    cover(y0,M(1)/8,M(K),threshold)
    # At the right endpoint a fixed finite point beats all k <= d.
    c1, b1 = IV('4.932'), IV('-2.624')
    val = terms(IV(1),c1,b1,y1,weights,iv)[0]
    extra = terms(IV(1),c1,b1,[IV(-1)],[IV(1)],iv)[0]
    assert ((1-w)*val+w*extra-pareto(y1)).a > IV('0.003')
    print("finite/finite witness gap over bulk Pareto >",float(witness_gap.a),
          "separator deficit <",float(entropy_deficit.b),flush=True)


def signed_seam(y, radius, cutoff):
    lower, upper, power, v = weibull_bracket(y,radius)
    assert (lower-pareto(y)).a > IV('0.05')
    first = avg([u*u for u in v])/2-1
    assert first.b < 0
    # The signed seam theorem in the preceding root-box derivation,
    # with all information matrices and derivatives in mean units.
    r = IV(1)/16
    h = IV(1)/cutoff
    assert r < power.a
    norm = [t+1 for t in y]
    omega = [IV(u.a)*iv.exp(-r*z)/(1+h*IV(u.b)*iv.exp(r*z))**2
             for u,z in zip(v,norm)]
    aa = 1/(IV(power.b)+r)**2+avg([o*t*t for o,t in zip(omega,y)])
    bb = avg([o*t for o,t in zip(omega,y)])
    cc = avg(omega)
    mu = IV(((aa*cc-bb*bb)/(aa+cc)).a)
    assert mu > 0
    d0 = avg([u*(u-1) for u in v])
    d1 = avg([t*u*(u-1) for t,u in zip(y,v)])
    error = avg([u*u*abs(u-1)*z for u,z in zip(v,norm)])
    D = abs(d0)+abs(d1)+h*error
    B = IV(1)/6+D**2/(2*mu)
    assert (h*D/(mu*r)).b < IV('0.5')
    assert (h*B/(-IV(first.b))).b < IV('0.5')
    return lower,upper, first


def finite_weibull():
    K = 65536
    worst = -1
    # Uniform data and Weibull parameter enclosures for the deformation.
    for i in range(9000,10000):
        t = box(M((IV(i)/10000).a),M((IV(i+1)/10000).b))
        _,_,first = signed_seam(weibull_family(t),IV('0.003'),K)
        worst = max(worst,float(first.b))
    y = weibull_family(IV(9)/10)
    endpoint_cutoff = 8192
    lower,_,_ = signed_seam(y,IV('1e-12'),endpoint_cutoff)
    cover(y,M(1)/8,M(endpoint_cutoff),lower.a)
    y = weibull_family(IV(1))
    _, upper, _ = signed_seam(y,IV('1e-12'),K)
    val = terms(IV(1),IV('5.16'),IV('-2.816'),y,weights,iv)[0]
    assert (val-upper).a > IV('0.009')
    print("finite/Weibull upper seam slope",worst,
          "endpoint finite gap lower",float((val-upper).a),flush=True)


if __name__ == "__main__":
    two_finite()
    finite_weibull()
