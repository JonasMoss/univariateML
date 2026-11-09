"""80-digit independent inverse/score and Newton correction checks."""
from pathlib import Path
import json,mpmath as mp
mp.mp.dps=80
p=Path(__file__).resolve().parent
repo=p.parents[3]
x=[mp.mpf(v) for v in json.loads((repo/'univariateML-cpp/include/cppunivariate/distributions/tlambda/golden.json').read_text())['data']]
log2=mp.log(2)
def evaluate(m,s,l):
    ll=-len(x)*mp.log(s);g=[mp.mpf(0) for i in range(3)]
    for v in x:
        z=(v-m)/s;az=abs(z)
        a=(mp.log(1-l*az)-log2)/l if l>0 else -mp.log(1-l*az+mp.exp(-l*log2))/(-l)
        b=-log2;t=(a+b)/2
        for k in range(300):
            u=mp.exp(t);w=1-u
            h=mp.expm1(l*(mp.log(w)-t))*mp.exp(l*t)/l
            D=u**(l-1)+w**(l-1)
            f=h-az
            if abs(f)<mp.mpf('1e-72')*(1+az):break
            if f>0:a=t
            else:b=t
            nt=t+f/(u*D)
            t=nt if a<nt<b else (a+b)/2
        else:raise AssertionError('inverse did not converge')
        Lu=mp.log(w);Lv=t
        Du=(l-1)*(w**(l-2)-u**(l-2))
        Dl=w**(l-1)*Lu+u**(l-1)*Lv
        Ql=(w**l*Lu-u**l*Lv)/l-(w**l-u**l)/l**2
        psi=-mp.sign(z)*Du/D**2
        ll-=mp.log(D)
        g[0]-=psi/s;g[1]+=(-1-z*psi)/s;g[2]+=-Dl/D+Du*Ql/D**2
    return ll,mp.matrix(g)
if __name__=='__main__':
    results=[]
    for line in (p/'golden.log').read_text().splitlines():
        mask,status,*values=line.split();assert status=='0'
        theta=list(map(lambda v:mp.mpf(float(v)),values[:3]));ll,g=evaluate(*theta)
        indices=[j for j in range(3) if not (int(mask)==1 and j==0) and not (int(mask)==2 and j==1)]
        H=mp.matrix(len(indices));h=mp.mpf('1e-25')
        for col,j in enumerate(indices):
            left=theta.copy();right=theta.copy();left[j]-=h;right[j]+=h
            gl=evaluate(*left)[1];gr=evaluate(*right)[1]
            for row,i in enumerate(indices):H[row,col]=(gr[i]-gl[i])/(2*h)
        correction=mp.lu_solve(H,-mp.matrix([g[i] for i in indices]))
        relative=[abs(correction[k]/theta[j]) for k,j in enumerate(indices)]
        assert max(relative)<mp.mpf('1e-10')
        assert abs(ll-mp.mpf(float(values[3])))<mp.mpf('2e-10')
        results.append(dict(mask=int(mask),logLik=mp.nstr(ll,70),free_scores=[mp.nstr(g[j],30) for j in indices],relative_newton_corrections=[mp.nstr(v,30) for v in relative]))
    (p/'golden-checks.json').write_text(json.dumps(results,indent=2)+'\n')
    print(json.dumps(results,indent=2))
