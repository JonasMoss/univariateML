"""Independent 80-digit log-coordinate scores on exact binary64 golden inputs."""
import json
from pathlib import Path
import mpmath as mp
mp.mp.dps=80
out=Path(__file__).resolve().parent
records={}
for line in (out/'roots.txt').read_text().splitlines():
    name,*values=line.split()
    fitted=[mp.mpf(float(v)) for v in values] # exact binary64 output values
    g=json.loads(Path(f'univariateML-cpp/include/cppunivariate/distributions/{name}/golden.json').read_text())
    x=list(map(mp.mpf,g['data'])) # exact conversion of binary64 floats
    inverse=name=='invburr'
    threshold=mp.mpf(0 if inverse else g['estimates']['min'])
    # Reproduce the delegate's binary64 transformed observations exactly.
    y=[mp.mpf(1/float(v)) if inverse else mp.mpf(float(v)-float(threshold)) for v in x]
    logs=list(map(mp.log,y)); n=len(y)
    def score(a,b,s):
        k,c=mp.exp(a),mp.exp(b)
        z=[c*(v-s) for v in logs]
        p=[1/(1+mp.exp(-v)) for v in z]
        return (n-k*sum(mp.log(1+mp.exp(v)) for v in z),n+sum(v-(k+1)*v*t for v,t in zip(z,p)),c*((k+1)*sum(p)-n))
    start=[mp.log(mp.mpf(g['estimates'][v])) for v in ['shape1','shape2','scale']]
    if inverse:start[2]=-start[2]
    a,b,s=mp.findroot(score,start,tol=mp.mpf('1e-65'))
    root=[mp.exp(a),mp.exp(b),mp.exp(-s if inverse else s)]
    jac=-2*sum(mp.log(v) for v in x) if inverse else 0
    k,c=mp.exp(a),mp.exp(b)
    ll=n*(a+b)-sum(logs)+sum(c*(v-s)-(k+1)*mp.log(1+mp.exp(c*(v-s))) for v in logs)+jac
    drift=[float(root[i]/mp.mpf(g['estimates'][v])-1) for i,v in enumerate(['shape1','shape2','scale'])]
    error=[float(fitted[i]/root[i]-1) for i in range(3)]
    fs=score(mp.log(fitted[0]),mp.log(fitted[1]),mp.log(1/fitted[2] if inverse else fitted[2]))
    rec={'root':[mp.nstr(v,60) for v in root], 'certified_relative_error':error,'golden_relative_drift':drift,'logLik':mp.nstr(ll,60),'certified_logLik_error':float(fitted[3]-ll),'certified_total_log_score_norm':float(mp.norm(fs))}
    assert max(map(abs,error))<1e-10
    assert abs(fitted[3]-ll)<1e-8
    assert mp.norm(fs)<mp.mpf('1e-7')
    records[name]=rec
(out/'verification.json').write_text(json.dumps(records,indent=2)+'\n')
print(json.dumps(records,indent=2))
