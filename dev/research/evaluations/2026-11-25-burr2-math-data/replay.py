"""Independent 80-digit root locations for the proved transitions."""
from certify import M, mp, numerical_weights, terms, weibull


def pareto_logs(q):
    return [M(0), M('0.23826')*q, 7*q/15, 8*q/15, 8*q/15, M(1), M(1)]


def top_logs(t):
    return [M(0), 1-10*t/15, 1-8*t/15, 1-7*t/15, 1-7*t/15, M(1), M(1)]


def print_row(label, *values):
    print(label, *(mp.nstr(value, 22) for value in values), flush=True)


def finite_pair():
    w = 1/(7*M(2)**90+1)
    weights = [(1-w)/7]*7+[w]

    def small_fit(q):
        y = pareto_logs(q)
        d = sum(y)/7

        def trial(logk):
            k = mp.exp(logk)
            p0 = 7/((1+k)*(1-w))-6
            b = mp.log(p0/(1-p0))
            c = 1/((1-w)*k*d+w)
            # This proposal uses saturated logits off the bulk minimum.
            g = w*c-k*(1-w)*(b+mp.log1p(mp.exp(-b))/7)
            return g,k,c,b

        logk = mp.findroot(lambda u: trial(u)[0], (-35,-30), tol=M('1e-145'))
        _,k,c,b = trial(logk)
        val, score, _ = terms(k,c,b,y+[M(-1)],weights)
        assert max(map(abs,score)) < M('1e-60')
        return (k,c,b),val

    def system(q,k,c,b):
        val, score, _ = terms(k,c,b,pareto_logs(q)+[M(-1)],weights)
        return *score, val-small_fit(q)[1]

    q,k,c,b = mp.findroot(system, (M('.993'),M('.808'),M('5.366'),M('-2.489')),
                          tol=M('1e-145'))
    left, value = small_fit(q)
    bulk_y = pareto_logs(q)
    p = -1-mp.log(1+(1-w)*sum(bulk_y)/7)
    W = weibull(bulk_y+[M(-1)],weights)[1]
    assert 0<q<1 and left[0]<M(1)/8<k
    assert value>max(p,W)
    assert max(map(abs,system(q,k,c,b)))<M('1e-61')
    print_row("finite/finite branch crossing q, P gap, W gap",q,value-p,value-W)
    print_row("lower-k stationary k,c,b",*left)
    print_row("upper-k stationary k,c,b",k,c,b)


def finite_face():
    def system(t,k,c,b):
        y = top_logs(t)
        val, score, _ = terms(k,c,b,y,numerical_weights)
        return *score, val-weibull(y,numerical_weights)[1]

    t,k,c,b = mp.findroot(system, (M('.97'),M('.9'),M(5),M(-3)),
                          tol=M('1e-145'))
    y = top_logs(t)
    _, W, A = weibull(y,numerical_weights)
    p = -1-mp.log(sum(y)/7)
    assert M('.9')<t<1 and A<0 and W>p
    assert max(map(abs,system(t,k,c,b)))<M('1e-62')
    print_row("finite/Weibull branch crossing t,k,c,b, W-P, seam",t,k,c,b,W-p,A)


if __name__ == "__main__":
    finite_pair()
    finite_face()
