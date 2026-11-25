"""Check cheap face comparisons and the lifted paralogistic examples."""
import json
import math

from check import DIST, M, IV, mp, fmt, terms, weibull, weibull_upper, lomax, bisect, fixed


def golden_bounds(name):
    data=json.loads((DIST/name/'golden.json').read_text())
    x=[M(float(v)) for v in data['data']]
    sign=1 if name=='burr' else -1
    yi=[sign*mp.iv.log(IV(v)) for v in x]
    y=[sign*mp.log(v) for v in x]
    n=len(y)
    wi,w=[IV(1)/n]*n,[M(1)/n]*n
    params=data['estimates']
    k,c,s=[M(float(params[key])) for key in ['shape1','shape2','scale']]
    # b is an arbitrary feasible dyadic proposal. The analytic
    # optimum is unnecessary in a strict witness test.
    b=-sign*c*mp.log(s)
    val=terms(IV(k),IV(c),IV(b),yi,wi,mp.iv)[0]
    imin=y.index(min(y))
    P=-mp.iv.log(sum(a*(u-yi[imin]) for a,u in zip(wi,yi)))-1
    W=weibull_upper(yi,wi,weibull(y,w)[0])
    assert val.a>P.b and val.a>W
    print(name, 'interval witness lower gaps versus P,W',
          float((n*(val-P)).a),float((n*(val-W)).a))


def signed_lomax_tail():
    w=[M('.5')]*2
    for ratio in [M('1'),M('1.999999999999'),M(2),M('28.81'),M(100)]:
        data=[2/(1+ratio),2*ratio/(1+ratio)]
        y=[mp.log(v) for v in data]
        a=sum(v*v for v in data)/4
        cutoff=min(1/(2*a),(1-a)/(2*a*(1+a)))
        for factor in [M('.001'),M('.1'),M(1)]:
            rate=cutoff*factor
            gap=lomax(y,w,mp.log(rate))[1]
            bound=-(1-a)*rate+(a*a+a)*rate*rate
            assert gap<bound<0
        if ratio<=2:
            for i in range(-40,41):
                assert lomax(y,w,M(i)/2)[0]<0
        print('two-point signed tail',fmt(ratio,a,cutoff))
    for ratio,positive in [('2',False),('100',True)]:
        z=mp.iv.log(IV(ratio))
        _,gap,_=lomax([IV(0),z],[IV(1)/2]*2,-mp.iv.log(IV(2)),mp.iv)
        assert gap.a>0 if positive else gap.b<0
        print('two-point rate 1/2 witness gap',ratio,gap)


def lifted():
    w=[M('.5')]*2
    for r in [M('.003'),M('.004'),M('.005')]:
        y=[-M('.001'),-r]
        def score(c):
            G=sum(mp.log1p(mp.exp(c*v)) for v in y)/2
            k=1/G
            return terms(k,c,M(0),y,w)[1][1]
        c=bisect(score,M(1),M(10000),180)
        G=sum(mp.log1p(mp.exp(c*v)) for v in y)/2
        print('lifted held-scale paralogistic',fmt(r,1/G,c))
    y=[M(0),M(1)]
    w=[M(1)/10000,M(9999)/10000]
    _,W,A=weibull(y,w)
    for k in [M('.01'),M(1),M(100)]:
        p=1/((1+k)*w[1])
        power=1/w[0]
        c,b=fixed(y,w,k,(power,mp.log(p/(1-p))-power))
        print('lifted two-level paralogistic all-free',fmt(k,c,b,terms(k,c,b,y,w)[0]-W,A))


def rounded_maps():
    a=1.75
    for _ in range(64):
        b=math.nextafter(a,math.inf)
        if 1/a==1/b:
            ya,yb=[-mp.iv.log(IV(M(v))) for v in [a,b]]
            assert ya.a>yb.b
            print('reciprocal rounding collision',a.hex(),b.hex(),(1/a).hex(),ya-yb)
            break
        a=b
    else:
        raise AssertionError('expected adjacent reciprocal collision')
    a,b,mu=1.,math.nextafter(1.,math.inf),-float(2**54)
    assert (a-mu)==(b-mu)
    e0,e1=IV(M(a))-IV(M(mu)),IV(M(b))-IV(M(mu))
    direct=mp.iv.log(e1)-mp.iv.log(e0)
    stable=mp.iv.log1p((IV(M(b))-IV(M(a)))/e0)
    assert stable.a>0 and (direct-stable).a<=0<=(direct-stable).b
    print('threshold rounding collision',a.hex(),b.hex(),mu.hex(),stable)


if __name__=='__main__':
    for name in ['burr','invburr']:
        golden_bounds(name)
    signed_lomax_tail()
    lifted()
    rounded_maps()
