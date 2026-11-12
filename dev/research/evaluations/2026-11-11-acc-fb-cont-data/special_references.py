from pathlib import Path
import json,mpmath as mp
P=Path(__file__).resolve().parent
mp.mp.dps=420
cases={str(c['index']):c for c in json.loads((P/'cases.json').read_text())}
c=cases['426'];x=list(map(mp.mpf,c['data']));mu=mp.fsum(x)/len(x);a=mp.mpf(c['held']['a']);sigma=mp.mpf(c['held']['sigma'])
# For Gaussian + Uniform[-a,a], the strictly decreasing location score has
# mu = mean(x) - mean(E[U|x]), |U|<=a. It has exactly one zero.
lo,hi=mu-a,mu+a
assert mp.nstr(lo,65)==mp.nstr(hi,65)
r={'id':'426','family':'bhatt','digits':420,'type':'exact stationary enclosure','reference':{'mu':mp.nstr(mu,65),'sigma':mp.nstr(sigma,65),'a':mp.nstr(a,65)},'location_lower':mp.nstr(lo,410),'location_upper':mp.nstr(hi,410),'relative_reference_radius':mp.nstr(a/max(abs(mu),sigma),70),'proof':'The conditional latent uniform location score identity and strict log concavity enclose the unique stationary point. Both endpoints round to the same 65 significant digits.'}
(P/'special-references.json').write_text(json.dumps([r],indent=2)+'\n');print(r['reference'],r['relative_reference_radius'])
c=cases['sn-audit-6448'];x=list(map(mp.mpf,c['data']));xi=mp.mpf(c['held']['xi']);w=mp.mpf(447214.34522771905);a=mp.mpf(1126792.1138849647)
mp.mp.dps=100
def logphi(z):return mp.log(mp.erfc(-z/mp.sqrt(2))/2)
def f(u,v):
 omega=w*mp.exp(u);alpha=a*mp.exp(v)
 return mp.fsum(-mp.log(omega)-(z-xi)**2/(2*omega**2)+logphi(alpha*(z-xi)/omega) for z in x)
def g(u,v):return mp.diff(lambda t:f(t,v),u),mp.diff(lambda t:f(u,t),v)
u,v=mp.findroot(g,(0,0),tol=mp.mpf('1e-70'),maxsteps=40)
r2={'id':'sn-audit-6448','family':'sn','digits':100,'type':'independent differentiated log density','reference':{'xi':mp.nstr(xi,70),'omega':mp.nstr(w*mp.exp(u),70),'alpha':mp.nstr(a*mp.exp(v),70)},'residual':mp.nstr(max(map(abs,g(u,v))),20)}
(P/'special-references.json').write_text(json.dumps([r,r2],indent=2)+'\n');print(r2['reference'])
