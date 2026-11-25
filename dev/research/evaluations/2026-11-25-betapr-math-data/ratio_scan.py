"""Scalar optimizations in the fixed-power harmonic shape chart."""
import mpmath as mp
from accuracy import scores

def optimum(logs, weights, r):
    mean = lambda f: mp.fsum(w*f(y) for y,w in zip(logs,weights))/sum(weights)
    def mode_score(m):
        return mean(lambda y: mp.expm1(y-m)/(1+r*mp.expm1(y-m)))
    lo, hi = min(logs), max(logs)
    for _ in range(mp.mp.prec):
        mid = (lo+hi)/2
        if mode_score(mid)>0:
            lo = mid
        else:
            hi = mid
    m = (lo+hi)/2
    if r == 0:
        d = mean(lambda y: mp.expm1(y-m)-(y-m))
        coeffs = (mp.mpf(1),)
        total = None
    elif r == 1:
        return optimum([-y for y in logs],weights,mp.mpf(0))
    else:
        d = mean(lambda y: (mp.log1p(r*mp.expm1(y-m))-r*(y-m))/(r*(1-r)))
        coeffs = (1/(1-r),1/r)
        total = 1/(r*(1-r))
    def stirling(z):
        return mp.loggamma(z)-(z-mp.mpf('.5'))*mp.log(z)+z-mp.log(2*mp.pi)/2
    def deriv(z):
        return mp.digamma(z)-mp.log(z)+1/(2*z)
    def score(k):
        return (1/(2*k)-mp.fsum(c*deriv(c*k) for c in coeffs)
                +(total*deriv(total*k) if total else 0)-d)
    lo,hi = 1/(2*d), 2/d
    while score(lo)<0:
        lo/=2
    assert score(hi)<0
    for _ in range(mp.mp.prec):
        k=(lo+hi)/2
        if score(k)>0:
            lo=k
        else:
            hi=k
    k=(lo+hi)/2
    value = (mp.log(k/(2*mp.pi))/2-mp.fsum(stirling(c*k) for c in coeffs)
             +(stirling(total*k) if total else 0)-k*d)
    return value

def replay(precision):
    with mp.workdps(precision):
        T=512*mp.log(2)
        grid=tuple(map(mp.mpf,('0','.025','.05','.1','.15','.2','.3','.4','.5')))
        cases={
            'wide tie': ([-T,-T/2,T/2,T],[1,10,10,1]),
            'balanced faces': ([-T,T],[1,1]),
            'narrow balanced': ([-mp.log1p(mp.mpf(1)/2**20),mp.log1p(mp.mpf(1)/2**20)],[1,1]),
            'wide singleton': ([-mp.mpf(10)**6,mp.mpf(0),mp.mpf(10)**6],[1,4,1]),
        }
        answer={}
        for label,(logs,w) in cases.items():
            vals=[optimum(logs,w,r) for r in grid]
            diffs=[v-vals[0] for v in vals]
            answer[label]=diffs
            print(precision,label,'profile minus face:',*(mp.nstr(v,16) for v in diffs),flush=True)
            if label in ('balanced faces','narrow balanced'):
                assert max(diffs[1:])<0
            if label=='wide tie':
                assert max(diffs[1:-1])>max(0,diffs[-1])
            if label=='wide singleton':
                assert diffs[-1]>max(diffs[:-1])
        logs,w=cases['wide tie']
        expanded=[y for y,weight in zip(logs,w) for _ in range(weight)]
        def branch(lp,lq,offset):
            return scores(expanded,(mp.exp(lp),mp.exp(lq),mp.exp(T/2+offset)))
        lp,lq,offset=mp.findroot(branch,(mp.log(1.6/T),mp.log(7.6/T),mp.mpf(1)),
                                tol=mp.eps**mp.mpf('.75'))
        print(precision,'wide finite branch p q log(b):',
              *(mp.nstr(v,35) for v in (mp.exp(lp),mp.exp(lq),T/2+offset)),flush=True)
        answer['branch']=[lp,lq,offset]
        answer['crossing']=crossing()
        print(precision,'whole crossing t, P, Q, log(b)-tT, gamma shape:',
              *(mp.nstr(v,35) for v in answer['crossing']),flush=True)
        return answer


def crossing():
    T=mp.mpf(10)**6

    def equations(logP,logQ,h,t,logK):
        p,q,k=mp.exp(logP)/T,mp.exp(logQ)/T,mp.exp(logK)/T
        logb=t*T+h
        y=[-T]+[-t*T]*10+[t*T]*10+[T]
        z=[v-logb for v in y]
        j=T+mp.log(mp.fsum(mp.exp(v-T) for v in y)/22)
        sp,sq,sb=scores(y,(p,q,mp.exp(logb)))
        value=(-mp.loggamma(p)-mp.loggamma(q)+mp.loggamma(p+q)
               -mp.fsum(p*mp.log1p(mp.exp(-v))+q*mp.log1p(mp.exp(v)) for v in z)/22)
        face=k*(mp.log(k)-1-j)-mp.loggamma(k)
        return sp,sq,T*sb,value-face,k*(mp.log(k)-mp.digamma(k)-j)

    lp,lq,h,t,lk=mp.findroot(equations,(mp.log(mp.mpf('1.1')),mp.log(11),mp.log(9),
                                        mp.mpf(9)/11,mp.mpf(0)),
                             tol=mp.eps**mp.mpf('.8'))
    assert mp.mpf('.81')<t<mp.mpf('.83')
    return [t,mp.exp(lp),mp.exp(lq),h,mp.exp(lk)/T]


if __name__ == '__main__':
    low,high=replay(55),replay(85)
    with mp.workdps(85):
        for label in low:
            assert max(abs(a-b) for a,b in zip(low[label],high[label]))<mp.mpf('1e-35')
    print('profile replay passed')
