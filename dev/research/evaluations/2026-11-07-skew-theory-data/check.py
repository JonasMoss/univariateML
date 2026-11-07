"""Reproducible research checks, not changes to the library test suite.
Run from repository root through /tmp/umvl-cpu/run python3 <this file>.
Only the Fraction interval calculation below is an exact sign certificate.
"""
import json
import math
import platform
from fractions import Fraction as F
from pathlib import Path

import mpmath as mp
import scipy
from scipy import optimize, special, stats

mp.mp.dps = 70
OUT = Path(__file__).parent
records = {}

# Elementary rational interval operations; all endpoints are exact fractions.
def add(a, b): return a[0]+b[0], a[1]+b[1]
def neg(a): return -a[1], -a[0]
def mul(a, b):
    v = [x*y for x in a for y in b]
    return min(v), max(v)
def div(a, b):
    assert b[0] > 0
    return mul(a, (1/b[1], 1/b[0]))
def pt(x): return F(x), F(x)
def atan_interval(q, terms):
    # Alternating series for atan(1/q), remainder between consecutive sums.
    v = sum((F((-1)**j, (2*j+1)*q**(2*j+1)) for j in range(terms)), F(0))
    u = v+F((-1)**terms, (2*terms+1)*q**(2*terms+1))
    return min(u,v), max(u,v)

# Machin identity pi=16 atan(1/5)-4 atan(1/239).
pi = add(mul(pt(16), atan_interval(5, 48)), neg(mul(pt(4), atan_interval(239, 16))))
B = div(pt(F(99,5)), pi)  # nu=3, xi=10, mean=0: mu_u=(2/pi)*(10-.1).
old = json.loads(Path('dev/research/evaluations/2026-11-06-sstd-cert-data/skew-scale.json').read_text())
xs = [F(str(x)) for x in old['data']]
signs = []
for v in ['0.5', '0.15', '0.05', '0.015']:
    score = pt(-len(xs))
    for x in xs:
        y = pt(x*F(v))
        z = add(y, B)
        assert z[0]*z[1] > 0
        side2 = F(100) if z[0]>0 else F(1,100)
        den = add(pt(side2), mul(z,z))
        score = add(score, mul(pt(4), div(mul(y,z), den)))
    signs.append(dict(v=v, lower=float(score[0]), upper=float(score[1]),
                      exact_lower=str(score[0]), exact_upper=str(score[1])))
assert [r['lower']>0 for r in signs] == [True,False,True,False], signs
records['exact_sstd_scale_signs'] = signs
records['pi_interval_width'] = float(pi[1]-pi[0])

# Coordinate identity and public moment constraints, independent formulas.
errors = []
for nu in [2.01, 3., 9., 100.]:
    h = math.exp(special.gammaln((nu-1)/2)-special.gammaln(nu/2))/math.sqrt(math.pi)
    for skew in [.05,.7,1.,4.,50.]:
        M, S = .37, 1.4
        mu_u = h*math.sqrt(nu-2)*(skew-1/skew)
        su = math.sqrt(1+(1-(h*h*(nu-2)))*(skew-1/skew)**2)
        a, b = math.sqrt(nu-2)*S/su/skew, math.sqrt(nu-2)*S/su*skew
        c = M-h*(b-a)
        var = (a*a-a*b+b*b)/(nu-2)-h*h*(b-a)**2
        errors.extend([abs(c+h*(b-a)-M), abs(var-S*S)])
        K = 2*math.exp(special.gammaln((nu+1)/2)-special.gammaln(nu/2))/math.sqrt(math.pi)
        for x in [-5.,c,0.,2.]:
            z = su*(x-M)/S+mu_u
            side = skew if z>0 else 1/skew
            direct = math.log(su/S*2/(skew+1/skew))+special.gammaln((nu+1)/2)-special.gammaln(nu/2)-.5*math.log(math.pi*(nu-2))-(nu+1)/2*math.log1p((z/side)**2/(nu-2))
            width = b if x>c else a
            mapped = math.log(K/(a+b))-(nu+1)/2*math.log1p(((x-c)/width)**2)
            errors.append(abs(direct-mapped))
assert max(errors)<2e-11
records['sstd_coordinate_max_absolute_error'] = max(errors)

# Both held moments force a nontrivial thin-side endpoint layer at fixed nu.
nu=3.; h=2/math.pi; v=1-h*h; S=1.; M=0.
b0=S/math.sqrt(v); c0=M-h*b0; rho=h/(2*(nu-2)*v)
layers=[]
for a in [1e-3,1e-5,1e-7]:
    cross=2*h*h-1
    b=(-cross*a+math.sqrt(cross*cross*a*a-4*v*(v*a*a-S*S)))/(2*v)
    c=M-h*(b-a)
    layers.append(dict(a=a,mode_displacement_over_a=(c-c0)/a,
                       endpoint_density_ratio=(1+((c-c0)/a)**2)**(-2)))
assert abs(layers[-1]['mode_displacement_over_a']-rho)<1e-6
records['sstd_held_moments_endpoint_layer']={'rho':rho,'limit_ratio':(1+rho*rho)**(-2),'checks':layers}

# Diagnostic finite sample of the proved uniform t envelope; not its proof.
ratios=[stats.t.pdf(z,nu)*(1+z*z)**1.5 for nu in [2.,2.01,3.,10.,100.,1e5] for z in [0.,.1,1.,2.,10.,1000.]]
assert max(ratios)<3
records['uniform_t_envelope_sample_max']=max(ratios)

# Strict monotonicity of the standardized absolute moment and normal corner.
js=[]
for nu in [mp.mpf('2.01'),3,10,100,10000]:
    j=mp.sqrt(nu-2)*mp.gamma((nu-1)/2)/(mp.sqrt(mp.pi)*mp.gamma(mp.mpf(nu)/2))
    derivative=(1/(2*mp.mpf(nu-2))+(mp.digamma((nu-1)/2)-mp.digamma(mp.mpf(nu)/2))/2)
    assert derivative>0
    js.append(dict(nu=float(nu),j=str(j),log_j_derivative=str(derivative)))
records['standardized_absolute_moment_monotonicity']=js
HN=mp.sqrt(2/mp.pi); VN=1-HN*HN; rhoN=HN/(2*VN); cN=-HN/mp.sqrt(VN)
normal_layers=[]
for astr, nustr in [('0.001','1e8'),('0.00001','1e12')]:
    A=mp.mpf(astr); nu=mp.mpf(nustr)
    H=mp.sqrt(nu)*mp.gamma((nu-1)/2)/(mp.sqrt(mp.pi)*mp.gamma(nu/2))
    V=nu/(nu-2)-H*H; cross=2*H*H-nu/(nu-2)
    B=(-cross*A+mp.sqrt(cross*cross*A*A-4*V*(V*A*A-1)))/(2*V)
    c=-H*(B-A); layer=(c-cN)/A
    assert layer>rhoN-mp.mpf('0.01')
    normal_layers.append(dict(A=astr,nu=nustr,layer=str(layer)))
assert abs(mp.mpf(normal_layers[-1]['layer'])-rhoN)<mp.mpf('.001')
records['sstd_normal_corner']={'rho':str(rhoN),'endpoint_multiplier':str(mp.exp(-rhoN*rhoN/2)),'checks':normal_layers}

normal_errors=[]
for nu in [2.,10.,100.]:
    logmom=math.sqrt(special.polygamma(1,nu/2)+(special.digamma(nu/2)-math.log(nu/2))**2)
    bound=math.sqrt(2/nu)/math.sqrt(2*math.pi)+logmom/(math.e*math.sqrt(2*math.pi))
    error=max(abs(stats.t.pdf(z,nu)-stats.norm.pdf(z)) for z in [0.,.1,1.,3.,10.,100.])
    assert error<bound
    normal_errors.append(dict(nu=nu,sample_error=error,global_bound=bound))
records['global_normal_kernel_bound_checks']=normal_errors

# The newly reduced sstd equality face, including exactly held public moments.
equalities=[]
r=mp.mpf('.3')
for epsstr in ['.001','.000001','.000000001','1e-15']:
    eps=mp.mpf(epsstr); nu=2+eps
    h=mp.gamma((nu-1)/2)/(mp.sqrt(mp.pi)*mp.gamma(nu/2))
    K=2*mp.gamma((nu+1)/2)/(mp.sqrt(mp.pi)*mp.gamma(nu/2))
    W=mp.sqrt(eps/(r*r-r*(1-r)+(1-r)**2-eps*h*h*(1-2*r)**2))
    a=W*r; b=W*(1-r); c=-h*(b-a)
    logL=sum(mp.log(K/W)-(nu+1)/2*mp.log1p(((x-c)/(a if x<=c else b))**2) for x in [0,0,1])
    t=-(1-2*r); J=(1+(t/(1-r))**2)**(-mp.mpf('1.5'))
    face=(1-r)**3*J**2
    equalities.append(dict(eps=epsstr,likelihood=str(mp.exp(logL)),face=str(face),error=str(abs(mp.exp(logL)-face))))
assert mp.mpf(equalities[-1]['error'])<mp.mpf('1e-7')
records['sstd_free_df_equality_both_moments_held']=equalities

# st equality, alpha-infinite endpoint layer: free versus held location.
st_equalities=[]
for ostr in ['.001','1e-6','1e-9']:
    omega=mp.mpf(ostr); alpha=1/omega
    vals=[]
    for loc in [-omega**mp.mpf('1.5'),mp.mpf(0)]:
        L=mp.mpf(1)
        for x in [0,1]:
            z=(x-loc)/omega; arg=alpha*z*mp.sqrt(2/(1+z*z))
            T=(1+arg/mp.sqrt(arg*arg+2))/2
            L*=2/(mp.pi*(1+z*z))*T/omega
        vals.append(L)
    st_equalities.append(dict(omega=ostr,free_location=str(vals[0]),held_location=str(vals[1])))
assert abs(mp.mpf(st_equalities[-1]['free_location'])-4/mp.pi**2)<mp.mpf('1e-7')
assert abs(mp.mpf(st_equalities[-1]['held_location'])-2/mp.pi**2)<mp.mpf('1e-7')
records['st_equality_endpoint_layer']=st_equalities

# Conditional st alpha bracket and score sign beyond its proved cutoff.
alpha_checks = []
for nu in [.3,1.,3.,20.]:
    m=nu+1
    q=[z*math.sqrt((nu+1)/(nu+z*z)) for z in [-2.,-.2,.4,3.]]
    def score(alpha):
        return sum(v*math.exp(stats.t.logpdf(alpha*v,m)-stats.t.logcdf(alpha*v,m)) for v in q)
    def bound(qs):
        pos=[v for v in qs if v>0]; negs=[v for v in qs if v<0]
        cm=math.exp(stats.t.logpdf(0,m))
        base=math.sqrt(m)/min(abs(v) for v in qs if v)
        C=2*cm*m**((m+1)/2)*sum(v**(-m) for v in pos)
        return max(base, (4*C/(m*len(negs)))**(1/m))
    right, left=bound(q),bound([-v for v in q])
    assert score(right)<0 and score(-left)>0
    alpha_checks.append(dict(nu=nu,left=-left,right=right,score_left=score(-left),score_right=score(right)))
records['st_alpha_brackets']=alpha_checks

# log Student CDF is convex in the remote negative tail: no log-concavity proof.
m=4
z=mp.mpf(-100)
pdf=lambda t: mp.gamma((m+1)/2)/(mp.sqrt(m*mp.pi)*mp.gamma(m/2))*(1+t*t/m)**(-(m+1)/2)
cdf=mp.betainc(mp.mpf(m)/2,mp.mpf('.5'),0,m/(m+z*z),regularized=True)/2
R=pdf(z)/cdf
second=-R*((m+1)*z/(m+z*z)+R)
assert second>0
records['log_student_cdf_second_derivative_at_minus100_df4']=str(second)

# Exact two-global-maxima examples for both models at symmetric held skew.
records['location_counterexamples']={'sstd':{'data':[-2,2],'sd':1,'nu':3,'xi':1,'maximizers':['-sqrt(3)','sqrt(3)']},'st':{'data':[-2,2],'omega':1,'nu':1,'alpha':0,'maximizers':['-sqrt(3)','sqrt(3)']}}
inventory=(OUT/'subsets.md').read_text()
tables=inventory.split('## st:')
counts=[sum(line.startswith('| ') and not line.startswith('| Free subset') for line in part.splitlines()) for part in tables]
assert counts==[16,16], counts
proofs=(OUT/'proofs.md').read_text()
assert proofs.count('\\[')==proofs.count('\\]')
assert '\\tag{17}' in proofs and '\\tag{18}' in proofs and '## 14.' in proofs
records['document_checks']={'subset_rows':counts,'display_math_blocks':proofs.count('\\[')}
records['environment']={'python':platform.python_version(),'scipy':scipy.__version__,'mpmath':mp.__version__,'machine':platform.node()}
(OUT/'checks.json').write_text(json.dumps(records,indent=2)+'\n')
print(json.dumps({k:v for k,v in records.items() if k!='exact_sstd_scale_signs'},indent=2))
print('PASS: exact rational score signs + - + -; coordinates; skew brackets; normal bounds; endpoint layers; both equality faces; 32 subset rows')
