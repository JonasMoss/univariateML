"""Check tail coefficients, structural fixture margins and real crossings."""
from fractions import Fraction as Q
from math import lcm, prod
import mpmath as mp


def held_tail(x, weights, p):
    mean = lambda vals: sum(w*v for v,w in zip(vals,weights))/sum(weights)
    m = mean(x)
    m2,m3 = (mean([(v/m)**j for v in x]) for j in (2,3))
    a = p*(p*(m2-1)-1)/2
    b = (-p*(p-1)*(2*p-1)/12+p**3*m2/2-p**3*m3/3
         +p**3*(m2-1)**2/2)
    return a,b


def wide_bounds():
    iv = mp.iv
    iv.dps = 60
    log, sqrt = iv.log, iv.sqrt
    T = 512*log(2)
    P, Qshape = iv.mpf(8)/5, iv.mpf(38)/5
    h = P*Qshape/(P+Qshape)
    for spread in [T,iv.mpf(10)**6]:
        witness = log(h)-(23*P+Qshape)/44-(P+Qshape)*log(2)/spread
        face_upper = -1-log(1-log(22)/spread)
        center_upper = -1-log(2*(iv.mpf(6)/11-2*log(2)/spread))
        assert witness > max(face_upper,center_upper)
        print('wide finite/reflection',spread,witness,face_upper,center_upper)
    face_lower = -1-(1+log(T))/T
    restricted_upper = -1+log(iv.mpf(6)/7)+12*(16/T)**2
    assert restricted_upper < face_lower and log(4)+1-4 < face_lower
    assert 16/T < iv.mpf('0.1')
    print('balanced binary64 face tie',face_lower,restricted_upper)
    T = iv.mpf(10)**6
    face_lower = -1-(1+log(T))/T
    face_upper = -1-log(1-log(22)/T)
    assert log(16)+1-8 < face_lower
    assert 64/T < iv.mpf('0.001')
    assert -1-log(2*(iv.mpf(6)/11-2*log(2)/T)) < face_lower
    for rational_t in ('0.81','0.83'):
        t=iv.mpf(rational_t)
        R=(1-t)/22
        L=t+R
        s=sqrt(L)+sqrt(R)
        asymptote=-1-2*log(s)
        if rational_t=='0.81':
            P=1/(sqrt(L)*s)
            Qshape=1/(sqrt(R)*s)
            lower=asymptote-(P+Qshape)*log(2)/T
            assert lower > face_upper
            print('whole finite/face crossing left',lower,face_upper)
        else:
            correction=90*(64/T)**2
            upper=max(asymptote,-1+log(iv.mpf(45)/46),
                      -1-log(2*(2+20*t)/22))+correction
            assert upper < face_lower
            print('whole finite/face crossing right',upper,face_lower)
    for n,p,r in [(2,Q(1,10),6),(22,Q(1,1000),45)]:
        assert r>2*n
        assert r > (Q(1,4)+2*p*n*(1+2*n)+2*p*n)/((1-p*(n-1))/2)
    w=log(iv.mpf(3)/2)-1-6*log(2)/T
    c=iv.mpf('0.999')/6
    boundary=-1-2*log(sqrt(iv.mpf('1.005')/6)+sqrt(c))
    correction=90*(64/T)**2
    assert max(boundary,-1,log(iv.mpf('.24')/c)-1,
               log(3)-12*c)+correction < w
    assert log(16)+1-16/iv.mpf(3)<w
    assert 2*iv.exp(3)/(3*(1+iv.exp(3))**2) > iv.mpf('.03')
    assert 4/T**2+2/(iv.mpf('.06')*T) < iv.mpf('.48')/144
    # Uniform separation and concavity for the complete pair on [0.81,0.83].
    l0,r0=iv.mpf('.818'),iv.mpf('.0076')
    mlo,mhi=iv.mpf('.055'),iv.mpf('.15')
    rate_ratio=sqrt(r0)/(sqrt(l0)+sqrt(r0))
    assert mlo<rate_ratio<mhi
    cmin=mlo*l0+(1-mlo)*r0
    height=lambda m: log(m*(1-m)/(m*l0+(1-m)*r0))-1
    assert max(height(mlo),height(mhi),log(60*iv.mpf('.1275'))-60*cmin)+correction < face_lower
    assert 60*cmin>1 and 4*iv.exp(face_lower)>iv.mpf('1.4')
    assert (22*mlo-iv.mpf('1.001'))/10 > 1/(1+iv.exp(4))
    assert (22*mhi-iv.mpf('.999'))/10 < 1/(1+iv.exp(-4))
    assert 10*iv.exp(4)/(22*(1+iv.exp(4))**2)>iv.mpf('.008')
    assert 2*(60/T)**2+60/(iv.mpf('.008')*T)<2*mlo*(1-mlo)
    assert 1/(4*sqrt(l0))-1/(4*sqrt(iv.mpf('.00914'))) < -iv.mpf('2.3')
    assert 21/(44*sqrt(iv.mpf('.84')))-1/(44*sqrt(r0)) > iv.mpf('.25')
    assert sqrt(iv.mpf('.838'))+sqrt(iv.mpf('.00864'))<iv.mpf('1.009')
    assert (1/sqrt(l0)+1/sqrt(r0))/iv.mpf('.99')<15
    witness_floor=-1-2*log(iv.mpf('1.009'))-15*log(2)/T
    assert -1+log(iv.mpf(45)/46)+correction < witness_floor
    assert -1-log(4*(1+10*iv.mpf('.81'))/22)+correction < witness_floor
    assert 2*iv.mpf('.25')*iv.mpf('.001')/iv.mpf('1.02')>15*log(2)/T+correction


def unit_crossing(precision):
    with mp.workdps(precision):
        x0 = (mp.mpf(1)/10,mp.mpf(19)/9)
        w = (mp.mpf(100)/181,mp.mpf(81)/181)

        def equations(lograte, a):
            rate=mp.exp(lograte)
            x=[1+a*(v-1) for v in x0]
            ar=mp.fsum(weight*mp.log1p(rate*v) for v,weight in zip(x,w))
            u=mp.fsum(weight*rate*v/(1+rate*v) for v,weight in zip(x,w))
            return mp.log(rate/ar)-ar, ar-u/(1-u)

        candidates=[]
        for rate in ('0.1','0.3','1','3'):
            try:
                s,a=mp.findroot(equations,(mp.log(mp.mpf(rate)),mp.mpf('.99')),
                                maxsteps=60, tol=mp.eps**mp.mpf('.75'))
            except (ValueError,ZeroDivisionError,TypeError):
                continue
            if 0<a<1 and s>-8:
                candidates.append((mp.exp(s),a))
        assert candidates
        root=candidates[0]
        assert all(max(abs(a-b) for a,b in zip(root,other)) < mp.eps**mp.mpf('.5')
                   for other in candidates)
        print('unit crossing',precision,*(mp.nstr(v,43) for v in root))
        return root


def main():
    for x,w,p,sgn in [
            ((Q(1,8),Q(15,8)),(1,1),Q(64,49),1),
            ((Q(1,10),Q(19,9)),(100,81),Q(1),1),
            ((Q(1,2),Q(3)),(4,1),Q(1),-1)]:
        a,b = held_tail(x,w,p)
        assert a==0 and sgn*b>0
        print('held tail',x,w,p,'A=',a,'B=',b)
    for offsets, sign in [
            ((Q(-4,5),Q(-3,4),Q(5,4),Q(3,2)),-1),
            ((Q(-9,10),Q(-1,2),Q(1),Q(4)),1)]:
        weights=[]
        for u in offsets:
            v=[t for t in offsets if t!=u]
            weights.append((Q(1,2)-sum(v)-prod(v))/prod(u-t for t in v))
        assert all(w>=0 for w in weights)
        n=lcm(*(w.denominator for w in weights))
        counts=[int(n*w) for w in weights]
        x=[1+u for u in offsets]
        assert sum(weights)==1
        assert sum(w*u for w,u in zip(weights,offsets))==0
        assert held_tail(x,counts,Q(1))==(0,0)
        cubic=sum(w*u**4 for w,u in zip(weights,x))/4-Q(8,3)
        assert sign*cubic>0
        print('unit double cancellation', list(zip(x,counts)), 'cubic=',cubic)
    assert Q(2,27)>69*Q(1,2**20)
    wide_bounds()
    first=unit_crossing(65)
    second=unit_crossing(95)
    with mp.workdps(95):
        assert max(abs(a-b) for a,b in zip(first,second))<mp.mpf('1e-42')
    print('fixture checks passed')


if __name__=='__main__':
    main()
