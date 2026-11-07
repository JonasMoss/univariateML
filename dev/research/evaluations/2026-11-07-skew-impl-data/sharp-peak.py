"""Independent near-df-two location/likelihood accuracy regression.
Run with the compiled probe.cpp under a single CPU slot.
"""
import json,math,pathlib,subprocess,sys
import mpmath as mp
mp.mp.dps=70
nu=math.nextafter(2.,math.inf)
line=subprocess.run([sys.argv[1]],input=f'0 3 0 1 {nu!r} 1 -2 2 2.5\n',text=True,capture_output=True,check=True).stdout
status,cert,p,ll=map(float,line.split())
v=mp.mpf(nu);m=v-2;xs=list(map(mp.mpf,[-2,2,2.5]))
def score(mu):return sum((x-mu)/(m+(x-mu)**2) for x in xs)
r=mp.findroot(score,(mp.mpf(2),mp.mpf(2)+2*m),tol=mp.mpf('1e-60'))
ref=sum(mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(mp.pi*m)/2-(v+1)/2*mp.log1p((x-r)**2/m) for x in xs)
result=dict(nu=nu,status=status,certified=cert,estimate=p,reference_mean=float(r),loglik=ll,reference_loglik=float(ref),loglik_loss=float(ref)-ll,dps=70)
assert status==0 and cert==1 and abs(float(ref)-ll)<1e-10,result
pathlib.Path(__file__).with_suffix('.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result))
