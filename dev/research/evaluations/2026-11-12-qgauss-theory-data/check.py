"""Independent high-precision formula checks, not a fitter or global certificate."""
import json
from pathlib import Path
import mpmath as m
m.mp.dps = 80
D = Path(__file__).resolve().parent

def logc(e):
    if not e: return -m.log(2*m.pi)/2
    if e > 0: return m.log(e)/2-m.log(m.beta(m.mpf('.5'),1/(2*e)))
    p=-(1+e)/(2*e)
    return m.log(-e)/2-m.log(m.beta(m.mpf('.5'),p+1))

def lf(x,mu,s,e):
    z=(x-mu)/s
    if not e: return logc(e)-m.log(s)-z*z/2
    w=1+e*z*z
    if w <= 0: raise ValueError('checks use strict support interiors')
    return logc(e)-m.log(s)-(1+e)/(2*e)*m.log(w)

def st(v):
    if isinstance(v,dict): return {k:st(w) for k,w in v.items()}
    if isinstance(v,list): return [st(w) for w in v]
    return str(v)

out={'precision':m.mp.dps,'baseline':'af824a98d78f2f2325c7a675c20c84c45e6bb826'}
# Two-sided seam derivative includes the normalizing constant.
out['seam']=[]
z=m.mpf('1.7'); expected=(z**4-2*z*z-1)/4
for e in map(m.mpf,['-.0001','-.000001','.000001','.0001']):
    slope=(lf(z,0,1,e)-lf(z,0,1,0))/e
    out['seam'].append({'eta':e,'slope':slope,'error':slope-expected})
assert abs(out['seam'][1]['error']) < m.mpf('0.00001')
assert abs(out['seam'][2]['error']) < m.mpf('0.00001')
# Collapse rates: negative, equality, positive at fixed nu.
out['collapse']=[]
xs=list(map(m.mpf,['0','0','1','2','3']))
for nu in map(m.mpf,['.5',str(m.mpf(2)/3),'1']):
    vals=[sum(lf(x,0,s,1/nu) for x in xs) for s in [m.mpf('1e-12'),m.mpf('1e-18')]]
    measured=(vals[1]-vals[0])/m.log(m.mpf('1e-6'))
    power=nu*3-2
    assert abs(measured-power)<m.mpf('1e-20')
    out['collapse'].append({'nu':nu,'predicted_power':power,'measured_power':measured})
# Endpoint spike eta=-2, a -> min=0, b=4 fixed.
out['endpoint_spike']=[]
e=m.mpf(-2); p=-(1+e)/(2*e)
for eps in map(m.mpf,['1e-8','1e-16','1e-32']):
    a=-eps;b=m.mpf(4);h=(b-a)/2;mu=(a+b)/2;s=m.sqrt(-e)*h
    ell=sum(lf(x,mu,s,e) for x in [m.mpf(0),m.mpf(1),m.mpf(2)])
    out['endpoint_spike'].append({'epsilon':eps,'ell':ell,'ell_minus_m_log_epsilon':ell-p*m.log(eps)})
# Fixed bounded shape nuisance Hessian in (u=1/h,v=-mu/h).
xs=list(map(m.mpf,['-1','.2','1.3']))
p=m.mpf('.7');u=m.mpf('.3');v=m.mpf('.1')
def bounded(uu,vv):
    return len(xs)*m.log(uu)-len(xs)*m.log(m.beta(m.mpf('.5'),p+1))+p*sum(m.log(1-(uu*x+vv)**2) for x in xs)
# Explicit Hessian is also compared to numerical derivatives.
H=m.matrix([[-len(xs)/u**2,0],[0,0]])
for x in xs:
    t=u*x+v;c=-2*p*(1+t*t)/(1-t*t)**2
    H+=c*m.matrix([[x*x,x],[x,1]])
Hnum=m.matrix([[m.diff(lambda q:bounded(q,v),u,2),m.diff(lambda q:m.diff(lambda r:bounded(q,r),v),u)], [m.diff(lambda q:m.diff(lambda r:bounded(q,r),v),u),m.diff(lambda r:bounded(u,r),v,2)]])
assert m.norm(H-Hnum)<m.mpf('1e-65')
assert H[0,0]<0 and m.det(H)>0
out['bounded_hessian']={'uu':H[0,0],'uv':H[0,1],'vv':H[1,1],'determinant':m.det(H)}
# Exact normal moments for information, and beta endpoint integral threshold.
moment=lambda k: m.mpf(0) if k%2 else m.fac2(k-1) if k else m.mpf(1)
polys=[[0,1],[ -1,0,1],[-m.mpf(1)/4,0,-m.mpf(1)/2,0,m.mpf(1)/4]]
I=m.matrix([[sum(a*b*moment(i+j) for i,a in enumerate(p1) for j,b in enumerate(p2)) for p2 in polys] for p1 in polys])
assert I==m.matrix([[1,0,0],[0,2,2],[0,2,m.mpf('3.5')]])
out['normal_information']={'matrix':[list(I[i,:]) for i in range(3)],'determinant':m.det(I),'efficient_eta':I[2,2]-I[2,1]**2/I[1,1]}
# Uniform corner envelope: endpoints approach data at different rates as m -> 0.
out['uniform_corner']=[]
for p in map(m.mpf,['.01','.001','.0001']):
    e=-1/(2*p+1)
    for c in map(m.mpf,['0','1','3']):
        eps=p if c==0 else m.exp(-c/p)
        # Avoid loss of epsilon by evaluating endpoint factors algebraically.
        h=1+eps
        logdensity=-m.log(h)-m.log(m.beta(m.mpf('.5'),p+1))+p*(m.log(eps)+m.log(2+eps)-2*m.log(h))
        out['uniform_corner'].append({'m':p,'c':c,'density_to_uniform':m.exp(logdensity+m.log(2)),'limit_ratio':m.exp(-c)})
# Held-mu, unbounded eta: per-point scale-max bound.
out['held_mu_large_eta']=[]
for e in map(m.mpf,['100','10000','1000000']):
    bound=m.exp(lf(1,0,1,e))
    out['held_mu_large_eta'].append({'eta':e,'eta_times_max_density_at_residual_one':e*bound})
# Existing t seam counterexample, directly re-evaluated in signed eta.
xs=[m.mpf(x)/100 for x in [-4,-3,-2,-1,1,2,3,4,-101,-99,99,101]]
sn=m.sqrt(sum(x*x for x in xs)/len(xs))
normal=sum(lf(x,0,sn,0) for x in xs)
candidate=sum(lf(x,0,m.mpf(1)/3,m.mpf(1)/3) for x in xs)
A=len(xs)*(sum((x/sn)**4 for x in xs)/len(xs)-3)/4
assert A<0 and candidate>normal
out['seam_counterexample']={'normal_slope_A':A,'finite_candidate_gain':candidate-normal,'eta':m.mpf(1)/3,'sigma':m.mpf(1)/3}
# Concave normalizer bound used by the bounded-branch profile chord.
out['profile_chord_curvature']=[]
for p in map(m.mpf,['0','.1','1','10','1000']):
    curvature=m.polygamma(1,p+1)-m.polygamma(1,p+m.mpf('1.5'))
    assert 0<curvature<m.pi**2/6
    out['profile_chord_curvature'].append({'m':p,'minus_A_second':curvature})
# Exact fixed-shape nuisance solution for the balanced two-point sample [-1,1].
# Its profile has the (k_min+k_max)*m*log(m) cusp at uniformity.
out['uniform_profile_cusp']=[]
for p in map(m.mpf,['.001','.000001','1e-12','1e-24']):
    gain=2*(m.log(2)-m.log(m.beta(m.mpf('.5'),p+1))-m.log(2*p+1)/2+p*(m.log(2*p)-m.log(2*p+1)))
    out['uniform_profile_cusp'].append({'m':p,'profile_minus_uniform':gain,'ratio_to_m_log_m':gain/(p*m.log(p))})
    assert gain<0
out['balanced_two_point']=[]
for e in map(m.mpf,['-.99','-.5','0','.25','1']):
    gain=2*(lf(1,0,1,e)+m.log(2))
    assert gain<0
    out['balanced_two_point'].append({'eta':e,'profile_minus_uniform':gain})
# Explicit signed uniform-neighborhood bounds, independent of a profile search.
out['uniform_signed_bounds']=[]
d=2-2*m.log(2)
for p in map(m.mpf,['.001','.000001','1e-12']):
    n=m.mpf(2); K=m.mpf(2); E=-2*m.log(2)
    A=n*m.log(2/m.beta(m.mpf('.5'),p+1))
    upper=A+n*m.log(n/(n+p*K))+p*K*m.log(4*p*K/(n+p*K))+p*E
    simple=p*(n*d+K*m.log(4*p*K/n)+E)
    exact=2*(m.log(2)-m.log(m.beta(m.mpf('.5'),p+1))-m.log(2*p+1)/2+p*(m.log(2*p)-m.log(2*p+1)))
    assert exact<=upper<=simple<0
    # Held mu, every residual has magnitude one: its upper bound is exact.
    n=m.mpf(3);K=n
    upper_mu=n*m.log(2/m.beta(m.mpf('.5'),p+1))+n/2*m.log(n/(n+2*p*K))+p*K*m.log(2*p*K/(n+2*p*K))
    e=-1/(2*p+1)
    exact_mu=n*(lf(1,0,1,e)+m.log(2))
    assert abs(upper_mu-exact_mu)<m.mpf('1e-65')
    out['uniform_signed_bounds'].append({'m':p,'two_point_exact_gain':exact,'all_free_upper':upper,'simple_upper':simple,'held_mu_exact_upper':upper_mu})
(D/'check.json').write_text(json.dumps(st(out),indent=2)+'\n')
print('PASS: two-sided seam, collapse exponents, endpoint spikes, nuisance Hessian, normal information, uniform corner and held-mu tail checks')
