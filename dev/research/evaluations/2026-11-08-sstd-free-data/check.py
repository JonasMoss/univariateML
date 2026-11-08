import json, pathlib, mpmath as mp
mp.mp.dps=70
root=pathlib.Path(__file__).resolve().parent
x=list(map(mp.mpf,[-2,-1,-.4,0,.2,.3,.5,1,2,5]))
def likelihood(q,nu,data=x,sd=mp.mpf(1)):
    m=mp.gamma((nu-1)/2)/mp.gamma(nu/2)*mp.sqrt((nu-2)/mp.pi)
    H=1-m*m+(4*m*m-3)*q*(1-q)
    c=mp.log(2*mp.sqrt(H)/sd)+mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(mp.pi*(nu-2))/2
    out=0
    for d in data:
        N=d*mp.sqrt(H)+m*(2*q-1)
        w=N/(q if N>0 else 1-q)
        out+=c-(nu+1)/2*mp.log1p(w*w/(nu-2))
    return out
count=0
for line in (root/'bounds.txt').read_text().splitlines():
    nu,l,h,vl,vh,sl,sh=map(mp.mpf,line.split())
    for j in range(1,10):
        q=l+(h-l)*j/10
        v=likelihood(q,nu);s=mp.diff(lambda t:likelihood(t,nu),q)
        assert vl<=v<=vh,(line,str(v))
        assert sl<=s<=sh,(line,str(s))
        count+=1
fits=[]
row=-1
for line in (root/'probe.txt').read_text().splitlines():
    if line.startswith(' candidate '): continue
    row+=1
    n,nu,status,cert,xi,ll,*_=line.split()
    if n!='10' or cert!='1' or status!='0':continue
    nu,xi,ll=map(mp.mpf,(nu,xi,ll));q0=xi*xi/(1+xi*xi)
    # Two different ten-observation samples occur in the probe.
    data=x if row<4 else list(map(mp.mpf,[-3,-1,-.5,0,.1,.2,.4,1,2,4]))
    q=mp.findroot(lambda q:mp.diff(lambda t:likelihood(t,nu,data),q),(q0-mp.mpf('1e-5'),q0+mp.mpf('1e-5')))
    true_xi=mp.sqrt(q/(1-q));true_ll=likelihood(q,nu,data)
    assert abs(true_xi-xi)<mp.mpf('1e-8'),(xi,true_xi)
    assert abs(true_ll-ll)<mp.mpf('1e-9'),(ll,true_ll)
    fits.append(dict(nu=str(nu),xi=str(true_xi),logLik=str(true_ll),xi_error=float(abs(true_xi-xi)),ll_error=float(abs(true_ll-ll))))
(root/'math-checks.json').write_text(json.dumps(dict(interval_checks=count,violations=0,roots=fits),indent=2)+'\n')
print(count,'interval checks; roots',len(fits))
# Frozen 500-observation sample: independent root in held mean=0,sd=1.4,nu=5.
golden=json.loads((root.parents[3]/'univariateML-cpp/include/cppunivariate/distributions/sstd/golden.json').read_text())
before=[line.split() for line in (root/'timing-before.txt').read_text().splitlines()]
after=[line.split() for line in (root/'timing-after.txt').read_text().splitlines()]
for a,b in zip(before,after):
    if a[0]!='7':assert a[2:]==b[2:],('unchanged route drift',a,b)
assert after[7][2:4]==['0','1'],after[7]
sd=mp.mpf(float(1.4));gx=[mp.mpf(v)/sd for v in golden['data']]
xi=mp.mpf(after[7][7]);q0=xi*xi/(1+xi*xi)
q=mp.findroot(lambda q:mp.diff(lambda t:likelihood(t,mp.mpf(5),gx,sd),q),(q0-mp.mpf('1e-5'),q0+mp.mpf('1e-5')))
true_xi=mp.sqrt(q/(1-q));true_ll=likelihood(q,mp.mpf(5),gx,sd)
assert abs(true_xi-xi)<mp.mpf('1e-8')
assert abs(true_ll-mp.mpf(after[7][8]))<mp.mpf('1e-9')
result=json.loads((root/'math-checks.json').read_text())
result['golden_xi_only']={'xi':str(true_xi),'logLik':str(true_ll),'xi_error':float(abs(true_xi-xi)),'ll_error':float(abs(true_ll-mp.mpf(after[7][8])))}
result['unchanged_routes_bitwise_equal']=15
(root/'math-checks.json').write_text(json.dumps(result,indent=2)+'\n')
print('golden xi-only check passed; 15 unchanged routes identical')
