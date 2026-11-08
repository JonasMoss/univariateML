# Numerical evidence for the exact fixed-mode uniqueness lemma in derivation.md.
import json, pathlib, mpmath as mp
mp.mp.dps=50
x=list(map(mp.mpf,[-2,-1,-.4,0,.2,.3,.5,1,2,5]))
rows=[]
for nu in map(mp.mpf,['2.1','5','20']):
    previous=mp.mpf(0)
    for c in map(mp.mpf,['-1','0.2','1']):
        left=[c-v for v in x if v<c];right=[v-c for v in x if v>c]
        C=len(x)/(nu+1)
        def T(side,scale):return sum(d*d/(nu*scale*scale+d*d) for d in side)
        def inverse(side,target):
            l,h=mp.mpf(-100),mp.mpf(100)
            assert T(side,mp.exp(l))>target>T(side,mp.exp(h))
            for _ in range(100):
                m=(l+h)/2
                if T(side,mp.exp(m))>target:l=m
                else:h=m
            return mp.exp((l+h)/2)
        l=max(mp.mpf(0),1-len(right)/C);h=min(mp.mpf(1),len(left)/C)
        assert l<h
        for _ in range(100):
            r=(l+h)/2;a=inverse(left,C*r);b=inverse(right,C*(1-r))
            if a/r>b/(1-r):l=r
            else:h=r
        r=(l+h)/2;a=inverse(left,C*r);b=inverse(right,C*(1-r))
        residual=max(abs((nu+1)*T(left,a)/a-len(x)/(a+b)),abs((nu+1)*T(right,b)/b-len(x)/(a+b)))
        assert residual<mp.mpf('1e-22'),residual
        assert r>previous
        previous=r
        def ll(a,b):return -len(x)*mp.log(a+b)-(nu+1)/2*(sum(mp.log1p(d*d/(nu*a*a)) for d in left)+sum(mp.log1p(d*d/(nu*b*b)) for d in right))
        best=ll(a,b)
        for u in map(mp.mpf,['.25','.5','1','2','4']):
            for v in map(mp.mpf,['.25','.5','1','2','4']):assert ll(a*u,b*v)<=best+mp.mpf('1e-40')
        rows.append(dict(nu=str(nu),mode=str(c),r=str(r),a=str(a),b=str(b),score_residual=float(residual)))
p=pathlib.Path(__file__).resolve().parent
(p/'side-scales.json').write_text(json.dumps(dict(pairs=rows,violations=0),indent=2)+'\n')
print('9 fixed-mode scale roots checked; monotone mode-to-r ordering; 225 grid comparisons')
