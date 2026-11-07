"""70-digit independent likelihood/score checks; run through the CPU slot lock.
Needs the existing mpmath and R BH headers; installs nothing. Writes results.json.
"""
import hashlib, json, math, os, pathlib, random, subprocess, time
import mpmath as mp
ROOT=pathlib.Path(__file__).resolve().parents[4]
HERE=pathlib.Path(__file__).resolve().parent
BUILD=ROOT/'build/scratch'
mp.mp.dps=70
no_long_double=os.environ.get("UMVL_CHECK_NO_LD")=="1"
exe=BUILD/("certificate-check-no-ld" if no_long_double else "certificate-check")
bh=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
subprocess.run(['c++','-std=c++17','-O2','-I'+str(ROOT/'univariateML-cpp/include'),'-I'+bh,*(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if no_long_double else []),str(HERE/'check.cpp'),'-o',str(exe)],check=True)
def fit(x,inverse=0,scale=0,a=-1,b=-1,maxit=100):
    text=' '.join(map(str,[inverse,scale,a,b,maxit,len(x),*x]))
    row=subprocess.check_output([str(exe)],input=text,text=True).split()
    return dict(status=int(row[0]),coef=list(map(float,row[1:4])),loglik=float(row[4]),evaluations=int(row[5]),budget=bool(int(row[6])))
def reference(x,r,inverse=0,held=()):
    k,p,s=map(mp.mpf,r['coef']); y=[(-1 if inverse else 1)*mp.log(v) for v in x]
    logs=(-1 if inverse else 1)*mp.log(s)
    z=[v-logs for v in y];v=[mp.exp(p*t) for t in z];n=len(x)
    ll=n*(mp.log(p)-mp.loggamma(k))+k*p*sum(z)-sum(v)-sum(mp.log(t) for t in x)
    score=[k*(p*sum(z)/n-mp.digamma(k)),1+k*p*sum(z)/n-sum(p*t*u for t,u in zip(z,v))/n,p*(sum(v)/n-k)]
    residual=max([abs(t) for i,t in enumerate(score) if i not in held] or [0])
    assert abs(ll-r['loglik'])<mp.mpf('2e-8')*(1+abs(ll)),(r,ll)
    assert residual<mp.mpf('1e-7'),(r,score)
    return dict(loglik_error=float(ll-r['loglik']),score_max=float(residual))
# Log collisions must not invent ties, and face witnesses use raw endpoints.
tiny=math.ulp(0.0);below=math.nextafter(7.0,0.0)
collisions=[fit([1,below,7]),fit([1,below,7],inverse=1),fit([below],scale=7)]
assert all(v['status']==6 for v in collisions),collisions
power=fit([7*((i+1)/400)**(1/2.5) for i in range(400)])
assert power['status']==2 and power['coef'][2]==7,power
pareto=fit([tiny,2*tiny,3*tiny],inverse=1)
assert pareto['status']==2 and pareto['coef'][2]==tiny,pareto
support_checks=dict(log_collision_statuses=[v['status'] for v in collisions],power_endpoint=power['coef'][2],subnormal_pareto_endpoint=pareto['coef'][2])
rows=[]
for inverse,family in enumerate(['trgamma','invtrgamma']):
    g=json.loads((ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())
    x=g['data'];r=fit(x,inverse)
    assert r['status']==0,r
    drift=max(abs(r['coef'][i]/v-1) for i,v in enumerate(g['estimates'].values()))
    assert drift<(2e-10 if inverse else 2e-11),(family,drift)
    rows.append(dict(case=family+' golden',**r,drift=drift,**reference(x,r,inverse)))
    for mask in range(1,8):
        values=list(g['estimates'].values())
        r=fit(x,inverse,values[2] if mask&4 else 0,values[0] if mask&1 else -1,values[1] if mask&2 else -1)
        assert r['status']==0,(family,mask,r)
        held=[i for i in range(3) if mask&(1<<i)]
        rows.append(dict(case=family+' held '+str(mask),**r,**reference(x,r,inverse,held)))
for x in [[.1,.3,1],[.1,.3,.7],[.5,.5,.5],[.1,2,4]]:
    r=fit(x,scale=1);assert r['status']==0,r
    k,p,_=map(mp.mpf,r['coef']);z=[mp.log(v) for v in x];n=len(x)
    root=mp.findroot(lambda a,b:(mp.digamma(a)-b*sum(z)/n,1+a*b*sum(z)/n-sum(b*v*mp.exp(b*v) for v in z)/n),(k,p))
    drift=max(abs(float(root[i])/r['coef'][i]-1) for i in range(2));assert drift<2e-11,drift
    inv=fit([1/v for v in x],inverse=1,scale=1);assert inv['status']==0,inv
    rows.append(dict(case='held endpoint '+str(x),**r,mp_root_drift=drift,**reference(x,r,held=(2,))))
rng=random.Random(11625)
for j in range(24):
    x=[math.exp(rng.uniform(-2,2)) for _ in range(12)]
    r=fit(x,scale=1)
    assert r['status']==0,(j,r)
    rows.append(dict(case='seeded held scale '+str(j),**r,**reference(x,r,held=(2,))))
# Exact high-precision profile roots in well-scaled coordinates.
from statistics import NormalDist
def mp_free_root(x,r):
    y=[mp.log(v) for v in x];n=len(y);mean=sum(y)/n
    sd=mp.sqrt(sum((v-mean)**2 for v in y)/n);u=[(v-mean)/sd for v in y]
    def cgf(t):
        w=[mp.exp(t*v) for v in u];total=sum(w)
        return mp.log(total/n),sum(v*a for v,a in zip(u,w))/total,sum(v*v*a for v,a in zip(u,w))/total
    def eq(a,t):
        K,K1,_=cgf(t)
        return mp.log(a)-mp.digamma(a)-K,1-a*t*K1
    a,t=mp.findroot(eq,(mp.mpf(r['coef'][0]),mp.mpf(r['coef'][1])*sd))
    K,K1,K2= cgf(t);p=t/sd;logscale=mean+(K-mp.log(a))/p
    value=n*(mp.log(p)+a*mp.log(a)-a-mp.loggamma(a)-a*K)-sum(y)
    return [a,p,mp.exp(logscale)],value
z=[NormalDist().inv_cdf((i+.5)/201) for i in range(201)]
x=[math.exp(.001*(v-.001*(v*v-1))) for v in z]
r=fit(x);assert r['status']==0,r
root,ll=mp_free_root(x,r)
drift=max(abs(float(v)/a-1) for v,a in zip(root,r['coef']))
assert drift<2e-11,drift
rows.append(dict(case='small-power represented normal seam',**r,mp_root_drift=drift,**reference(x,r)))
rounded=[]
for physical_sd in [1e-14,2e-14,5e-14]:
    x=[math.exp(physical_sd*(v-.1*(v*v-1))) for v in z]
    r=fit(x);assert r['status'] in [0,6],r
    root,maximum=mp_free_root(x,r)
    k,p,scale=map(mp.mpf,r['coef'])
    point=sum(mp.log(p)+k*p*mp.log(mp.mpf(v)/scale)-(mp.mpf(v)/scale)**p-mp.log(v)-mp.loggamma(k) for v in x)
    assert abs(point-r['loglik'])<mp.mpf('1e-7'),(physical_sd,point,r)
    gap=maximum-point
    if gap>2e-10*(1+abs(maximum)):assert r['status']==6,(physical_sd,gap,r)
    rounded.append(dict(physical_sd=physical_sd,status=r['status'],loglik_error=float(point-r['loglik']),gap_to_mp_root=float(gap)))
assert any(row['status']==6 for row in rounded),rounded
# A concrete density-evaluation gap outside this fitter task's dpqr scope.
density=fit([1],inverse=2,scale=1,a=1e-320,b=1)
held=fit([1],scale=1,a=1e-320,b=1)
assert density['loglik']==-math.inf and held['status']==6,(density,held)
evaluation_gaps=[dict(input=dict(x=1,shape1=1e-320,shape2=1,scale=1),actual='-Inf',expected=float(-1-mp.loggamma(mp.mpf(1e-320))),fitter_status=held['status'])]
# Validate two separated local maxima from the independent multiplicity scan.
multiple=[]
scan=json.loads((HERE/'multiplicity.json').read_text())
case=scan['candidates'][0];y=list(map(mp.mpf,case['logs']));n=len(y);mean=sum(y)/n
sd=mp.sqrt(sum((v-mean)**2 for v in y)/n);u=[(v-mean)/sd for v in y]
def profile_mp(t):
    w=[mp.exp(t*v) for v in u];total=sum(w);K=mp.log(total/n)
    a=mp.exp(mp.findroot(lambda b:b-mp.digamma(mp.exp(b))-K,mp.log(mp.mpf('.5')/K)))
    K1=sum(v*a for v,a in zip(u,w))/total
    K2=sum(v*v*a for v,a in zip(u,w))/total-K1*K1
    delta=mp.log(t)+a*mp.log(a)-a-mp.loggamma(a)-a*K+mp.log(2*mp.pi)/2+mp.mpf('.5')
    return a,delta,1/t-a*K1,-1/t**2+K1*K1/(mp.polygamma(1,a)-1/a)-a*K2
for l,h in case['brackets']:
    t=mp.findroot(lambda q:profile_mp(q)[2],(mp.mpf(l),mp.mpf(h)))
    a,delta,score,curvature=profile_mp(t)
    assert profile_mp(t*(1-mp.mpf('1e-7')))[2]>0
    assert profile_mp(t*(1+mp.mpf('1e-7')))[2]<0
    assert curvature<0
    multiple.append(dict(t=float(t),shape=float(a),delta=float(delta),curvature=float(curvature)))
seams=[]
# Independent high-precision evaluation of the uniform signed seam bound.
from statistics import NormalDist
for skew in [-.001,0,.001]:
    z=[mp.mpf(NormalDist().inv_cdf((i+.5)/41)) for i in range(41)]
    y=[v+mp.mpf(skew)*(v*v-1) for v in z];mean=sum(y)/len(y)
    sd=mp.sqrt(sum((v-mean)**2 for v in y)/len(y));u=[(v-mean)/sd for v in y]
    B=max(map(abs,u));T=mp.mpf('.01')/B;h=T*B
    third=sum(v**3 for v in u)/len(u);fourth=sum(v**4 for v in u)/len(u)
    c=fourth*mp.exp(-h)/12-mp.exp(2*h)/4;dmin=1-abs(third)*T/3-abs(c)*T*T
    C=-c/2+(abs(third)/3+abs(c)*T)**2/(4*dmin*dmin)-dmin/(6*(1+mp.sqrt(1+2*T*T*mp.exp(h)/3)))+T**4*mp.exp(3*h)/360
    A=-third/6;upper=max(0,A*T+C*T*T)
    if C<0 and A>0 and -A/(2*C)<T:upper=max(upper,-A*A/(4*C))
    actual=-mp.inf
    for j in range(1,13):
        t=T*j/12;K=mp.log(sum(mp.exp(t*v) for v in u)/len(u))
        a=mp.exp(mp.findroot(lambda b:b-mp.digamma(mp.exp(b))-K,mp.log(mp.mpf('.5')/K)))
        R=mp.loggamma(a)-(a-mp.mpf('.5'))*mp.log(a)+a-mp.log(2*mp.pi)/2
        delta=mp.log(t)+mp.log(a)/2-R-a*K+mp.mpf('.5')
        assert delta<=upper+mp.mpf('1e-50'),(skew,t,delta,upper)
        actual=max(actual,delta)
    seams.append(dict(skew=skew,T=float(T),upper=float(upper),sampled_max=float(actual),points=12))
files=['univariateML-cpp/include/cppunivariate/distributions/'+f+'/mle.hpp' for f in ['trgamma','invtrgamma']]
result=dict(precision=70,no_long_double=no_long_double,seed=11625,base_revision='3de415f0896527001caeb0d612bcb91b858d644d',source_sha256={f:hashlib.sha256((ROOT/f).read_bytes()).hexdigest() for f in files},checks=rows,support_checks=support_checks,seams=seams,represented_rounding=rounded,evaluation_gaps=evaluation_gaps,multiple_stationary_maxima=multiple)
(HERE/('results-no-long-double.json' if no_long_double else 'results.json')).write_text(json.dumps(result,indent=2,allow_nan=False)+'\n')
print(json.dumps(result))
print(f'{len(rows)} independent coefficient/likelihood/score checks, 36 seam inequalities, represented-rounding and multiplicity checks passed')
