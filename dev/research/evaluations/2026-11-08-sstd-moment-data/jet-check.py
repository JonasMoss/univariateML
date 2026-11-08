"""Generate independent natural-coordinate derivatives of log skew-t density."""
import pathlib,json,mpmath as mp
mp.mp.dps=100
out=pathlib.Path(__file__).resolve().parent
rows=[]
def f(mean,sd,nu,xi):
    m=mp.gamma((nu-1)/2)/mp.gamma(nu/2)*mp.sqrt((nu-2)/mp.pi)
    sig=mp.sqrt((1-m*m)*(xi*xi+1/(xi*xi))+2*m*m-1)
    z=-mean/sd*sig+m*(xi-1/xi)
    w=z/(xi if z>=0 else 1/xi)
    return mp.log(sig/sd)+mp.log(2/(xi+1/xi))+mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log((nu-2)*mp.pi)/2-(nu+1)/2*mp.log1p(w*w/(nu-2))
for nu in [128,129,10000,10**8,10**12,10**16]:
    p=(mp.mpf(0),mp.mpf(1),mp.mpf(nu),mp.mpf(2))
    grad=[mp.diff(f,p,tuple(int(i==j) for j in range(4))) for i in range(4)]
    hess=[mp.diff(f,p,tuple(int(i==k)+int(j==k) for k in range(4))) for i in range(4) for j in range(4)]
    rows.append(dict(nu=nu,value=mp.nstr(f(*p),80),gradient=[mp.nstr(v,80) for v in grad],hessian=[mp.nstr(v,80) for v in hess]))
(out/'jet-reference.json').write_text(json.dumps(rows,indent=2)+'\n')
(out/'jet-rows.txt').write_text('\n'.join(' {'+str(r['nu'])+', '+r['value']+', '+', '.join(r['gradient']+r['hessian'])+'},' for r in rows)+'\n')
