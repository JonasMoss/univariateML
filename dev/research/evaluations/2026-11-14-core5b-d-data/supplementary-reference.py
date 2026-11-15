"""120-digit independent roots and likelihoods, at exact binary64 inputs."""
import json,pathlib,mpmath as mp
mp.mp.dps=120
D=pathlib.Path(__file__).resolve().parent
out=[]
for mode in ['native','double']:
 for line in (D/f'supplementary-{mode}.txt').read_text().splitlines():
  name,*raw=line.split();a,b,ll=[mp.mpf(float.fromhex(s)) for s in raw[:3]];cert=bool(int(raw[3]))
  if name=='lgser':
   mean=mp.mpf(4294967293)/2
   u=mp.findroot(lambda u: mp.log(mp.expm1(u))-mp.log(u)-mp.log(mean),(24,25),maxsteps=200)
   reference=-mp.expm1(-u)
   likelihood=mp.fsum(mp.mpf(v)*mp.log(a)-mp.log(v)-mp.log(-mp.log1p(-a))+mp.log1p(-b) for v in [2147483646,2147483647])
   assert not cert
  elif name=='pareto':
   reference=1/mp.log(2)
   likelihood=mp.log(a)-(a+1)*mp.log(2)
   assert cert
  else:
   values=[mp.mpf('.25'),mp.mpf(2)] if name=='weibull' else [mp.mpf(1.5),mp.mpf(2./3)]
   logs=list(map(mp.log,values));first=mp.fsum(logs)
   guess=-first/mp.fsum(t*t for t in logs)
   reference=mp.mpf(1)/3 if name=='weibull' else mp.findroot(lambda k:-first-mp.fsum(t*mp.expm1(k*t) for t in logs),(guess/2,guess*2))
   likelihood=-mp.fsum(mp.exp(a*t) for t in logs)
   assert cert
  relative=abs(a/reference-1);error=abs(likelihood-ll)
  assert relative<mp.mpf('1e-10'),(mode,name,relative)
  assert error<mp.mpf('1e-11'),(mode,name,error)
  out.append(dict(mode=mode,family=name,held_second=float(b),reference_coefficient=mp.nstr(reference,100),returned_coefficient=float(a),relative_error=float(relative),reference_loglik=mp.nstr(likelihood,100),returned_loglik=float(ll),loglik_absolute_error=float(error),certified=cert))
assert len(out)==10
print(json.dumps(out,indent=2))
