"""Outward interval proof of an exact two-maximizer held-power sample."""
from check import M, IV, mp, box, lomax, bisect, fmt

WEIGHTS = [IV(1)/4]*4
BANDS = [('-4.56','-4.50'),('-1.39','-1.32')]


def data(r):
    return [IV(0), IV('5.1'), r, IV(8)]


def derivatives(y,t):
    z = [v+t for v in y]
    q = [1/(1+mp.iv.exp(v)) for v in z]
    G = sum(mp.iv.log1p(mp.iv.exp(v)) for v in z)/4
    Q = sum(q)/4
    C = sum(v*(1-v) for v in q)/4
    return (1-Q)*Q-(G+1)*C, -(1+1/G)*(1-q[2])/4


def cover():
    # All subdivision coordinates below are exact rationals or dyadics.
    # The decimal band endpoints are represented by precise dyadic proposals
    # and all evaluations use those same endpoints, without coverage gaps.
    bands = [(M(a),M(b)) for a,b in BANDS]
    outer = [(M(-16),bands[0][0]),(bands[0][1],bands[1][0]),(bands[1][1],M(16))]
    inspected = 0
    for j in range(30):
        r = IV('5.99')+IV([j,j+1])/1000
        y = data(r)
        lower = lomax(y,WEIGHTS,IV('-4.53'),mp.iv)[1].a
        assert lower > IV('.25')
        bounds = []
        for a,b in bands:
            assert lomax(y,WEIGHTS,IV(a),mp.iv)[0].a > 0
            assert lomax(y,WEIGHTS,IV(b),mp.iv)[0].b < 0
            bounds.append(derivatives(y,box(a,b))[1])
            for i in range(16):
                cell=box(a+(b-a)*i/16,a+(b-a)*(i+1)/16)
                assert derivatives(y,cell)[0].b < 0
        assert (bounds[1]-bounds[0]).a > 0
        # Right tail bound uses G(t)>=t+mean(y), so it decreases for t>=16.
        mean_y = sum(y)/4
        right = mp.iv.log(sum(mp.iv.exp(v) for v in y)/4)-mean_y-mp.iv.log(16+mean_y)
        assert right.b < 0
        assert (-mp.iv.log(1-mp.iv.exp(-8)/2)).b < IV('.001')
        stack = list(outer)
        while stack:
            a,b=stack.pop()
            inspected += 1
            assert inspected < 150000
            score,gap,_=lomax(y,WEIGHTS,box(a,b),mp.iv)
            if score.a>0 or score.b<0 or gap.b<lower:
                continue
            mid=(a+b)/2
            stack.extend([(a,mid),(mid,b)])
    return inspected


def difference(r):
    y=data(IV(r))
    intervals=[]
    for a,b in BANDS:
        a,b=M(a),M(b)
        for _ in range(55):
            mid=(a+b)/2
            val=lomax(y,WEIGHTS,IV(mid),mp.iv)[0]
            if val.a>0:
                a=mid
            else:
                assert val.b<0
                b=mid
        intervals.append(lomax(y,WEIGHTS,box(a,b),mp.iv)[1])
    return intervals[1]-intervals[0]


if __name__ == '__main__':
    print('held-power tie cover cells',cover())
    a,b=difference('5.99'),difference('6.02')
    assert a.b<0 and b.a>0
    print('held-power tie difference interval at 5.99 and 6.02',a,b)
    def system(tl,th,r):
        y=[M(0),M('5.1'),r,M(8)]
        w=[M('.25')]*4
        g1,l1,_=lomax(y,w,tl)
        g2,l2,_=lomax(y,w,th)
        return g1,g2,l2-l1
    tl,th,r=mp.findroot(system,(M('-4.53'),M('-1.36'),M('6.01')),tol=M('1e-65'))
    assert M('5.99')<r<M('6.02')
    for shift in [M('-1e-8'),M(0),M('1e-8')]:
        y=[M(0),M('5.1'),r+shift,M(8)]
        w=[M('.25')]*4
        roots=[bisect(lambda t:lomax(y,w,t)[0],M(a),M(b),220) for a,b in BANDS]
        values=[lomax(y,w,t)[1] for t in roots]
        delta=values[1]-values[0]
        assert abs(delta)<M('1e-65') if not shift else delta*shift>0
        print('held-power exact tie numerical replay',fmt(r+shift,*roots,*values,delta))
