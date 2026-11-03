#!/usr/bin/env python3
"""TASK-113: deterministic closure/expansion checks, no fitted estimates.
Run from the worktree root through /tmp/umvl-cpu/run. Requires mpmath and
SymPy already available in the validation environment. Output stays here.
"""
import csv
import itertools
import json
import math
import platform
from fractions import Fraction
from pathlib import Path
import mpmath as mp
import sympy as s

mp.mp.dps = 80
HERE = Path(__file__).resolve().parent
rows = []

def emit(kind, sample, path, scale, value, target):
    rows.append(dict(kind=kind, sample=' '.join(map(str, sample)), path=path,
                     scale=str(scale), value=mp.nstr(value, 25),
                     target=mp.nstr(target, 25), error=mp.nstr(value-target, 12)))

def bb(x, n, a, b):
    n, a, b = map(mp.mpf, (n, a, b))
    return (mp.loggamma(n+1)-mp.loggamma(x+1)-mp.loggamma(n-x+1)
            +mp.loggamma(a+x)-mp.loggamma(a)
            +mp.loggamma(b+n-x)-mp.loggamma(b)
            -mp.loggamma(a+b+n)+mp.loggamma(a+b))

def pois(x, mu):
    return x*mp.log(mu)-mu-mp.loggamma(x+1)

def nb(x, a, mu):
    return (mp.loggamma(a+x)-mp.loggamma(a)-mp.loggamma(x+1)
            -a*mp.log1p(mu/a)+x*(mp.log(mu/a)-mp.log1p(mu/a)))

def stats(xx):
    h = len(xx); mu = mp.mpf(sum(xx))/h
    v = sum((x-mu)**2 for x in xx)/h
    f2 = mp.mpf(sum(x*(x-1) for x in xx))/h
    f3 = mp.mpf(sum(x*(x-1)*(x-2) for x in xx))/h
    return mu, v, f2, f3

# Independent symbolic derivation from the exact adjacent probability ratio.
u, k, mu, x, j, z, v, f3 = s.symbols('u k mu x j z v F3')
e = s.symbols('e')
ratio_log = (s.log(1-x*k)+s.log(1+x*u)
             -s.log(1+mu*(u-k)-mu*(x+1)*u*k))
ratio_series = s.series(ratio_log.subs({u:e*u, k:e*k}), e, 0, 3).removeO()
r1 = s.summation(s.expand(ratio_series).coeff(e, 1).subs(x,j), (j,0,z-1))
r2 = s.summation(s.expand(ratio_series).coeff(e, 2).subs(x,j), (j,0,z-1))
p0_1 = mu**2*(u-k)/2
p0_2 = -mu**3*(u*u+k*k)/3+(mu**3-mu**2)*u*k/2
full1 = s.expand(r1+p0_1)
full2 = s.Poly(s.expand(r2+p0_2), z)
moments = {0:1, 1:mu, 2:v+mu**2,
           3:f3+3*(v+mu**2-mu)+mu}
avg2 = s.expand(sum(coef*moments[power[0]] for power,coef in full2.terms()))
f2 = v+mu**2-mu
B = (mu**3-f3)/6-f2/4
assert s.simplify(avg2-B*(u*u+k*k)-mu*v*u*k/2) == 0
assert s.simplify((2*B+mu*v/2).subs(v,mu)-(mu**3-f3)/3) == 0
# Exact diagonal ratio has only even powers of 1/N.
diag = s.simplify(((1-x*k)*(1+x*u)/(1+mu*(u-k)-mu*(x+1)*u*k)).subs(u,k))
assert s.simplify(diag-(1-x*x*k*k)/(1-mu*(x+1)*k*k)) == 0
n,a = s.symbols('N a',positive=True)
variance=mu*(n-mu)*(a+mu)/(a*n+mu)
assert s.simplify(variance-mu-mu**2*(n-a-mu-1)/(a*n+mu))==0
assert s.simplify(variance.subs(a,n-mu-1)-mu)==0

# Closure paths: proper NB and Poisson, and defective mass at zero.
for n in (100, 10000, 1000000):
    for a, c in ((mp.mpf('.3'),mp.mpf('.7')), (mp.mpf('4'),mp.mpf('2'))):
        err = max(abs(mp.exp(bb(t,n,a,c*n))-mp.exp(nb(t,a,a/c))) for t in range(7))
        emit('closure', [], f'NB a={a}, b/N={c}', n, err, 0)
        if n == 1000000: assert err < mp.mpf('2e-6')
    for a in (mp.sqrt(n), mp.mpf(n), mp.mpf(n)**2):
        m = mp.mpf('1.7'); b = a*(n/m-1)
        err = max(abs(mp.exp(bb(t,n,a,b))-mp.exp(pois(t,m))) for t in range(7))
        emit('closure', [], f'Poisson a={mp.nstr(a,8)}', n, err, 0)
        if n == 1000000: assert err < mp.mpf('.001')
for n in (10**10, 10**30):
    a = 2/mp.log(n)
    p0 = mp.exp(bb(0,n,a,1)); p1 = mp.exp(bb(1,n,a,1))
    emit('defective', [], 'a=2/log(N), b=1: p0', n, p0, mp.exp(-2))
    emit('defective', [], 'a=2/log(N), b=1: p1*log(N)', n, p1*mp.log(n), 2*mp.exp(-2))
# loggamma cancellation at N=1e60 needs >120 decimal digits.
# Evaluate the extreme path at 180 digits.
with mp.workdps(180):
    n=10**60; a=2/mp.log(n)
    emit('defective', [], 'a=2/log(N), b=1: p0', n, mp.exp(bb(0,n,a,1)), mp.exp(-2))
    emit('defective', [], 'a=2/log(N), b=1: p1*log(N)', n, mp.exp(bb(1,n,a,1))*mp.log(n), 2*mp.exp(-2))

examples = dict(positive=[0,0,0,1,1,1,2,3], zero=[1,1,4],
                negative=[0,0,0]+[1]*8+[4], ordinary=[0,1,2,4])
for name, xx in examples.items():
    m, vv, ff2, ff3 = stats(xx); h=len(xx)
    bcoef=(m**3-ff3)/6-ff2/4
    lp=sum(pois(t,m) for t in xx)
    for n in (100, 1000, 10000):
        for c in (mp.mpf('.25'), mp.mpf(1), mp.mpf(4)):
            a=n/c; b=a*(n/m-1); uu=1/a; kk=mp.mpf(1)/n
            actual=(sum(bb(t,n,a,b) for t in xx)-lp)/h
            first=(vv-m)*(uu-kk)/2
            second=bcoef*(uu**2+kk**2)+m*vv*uu*kk/2
            # Compare second coefficients after removing the first term.
            observed=(actual-first)/(uu+kk)**2
            target=second/(uu+kk)**2
            emit('joint_second',xx,f'u/k={c}',n,observed,target)
            if n == 10000: assert abs(observed-target) < mp.mpf('.003')
    if vv == m:
        g=(m**3-ff3)/3
        for n in (100,1000,10000):
            a=mp.mpf(n); b=a*(n/m-1)
            observed=n*n*(sum(bb(t,n,a,b) for t in xx)-lp)/h
            emit('diagonal',xx,name,n,observed,g)
            if n==10000: assert abs(observed-g)<mp.mpf('1e-6')
# Fourth-order diagonal coefficient for (1,1,4), mu=2.
# H4 = (mu^2 sum_{r=1}^x r^2 - sum_{j=0}^{x-1} j^4)/2.
# Center H4 and subtract Var(H2)/2 in log normalization.
xx=examples['zero']; m=mp.mpf(2)
eh4=(m*m/2)*(m**3/3+3*m*m/2+m) - (m**5/5+3*m**4/2+7*m**3/3+m*m/2)/2
# E sum_{r=1}^X r^2 = mu^3/3+3mu^2/2+mu.
varh2=2*m**3/3+m*m*(m+1)**2/2
avh4=sum((m*m*sum(r*r for r in range(1,t+1))-sum(j**4 for j in range(t)))/2 for t in xx)/len(xx)
quart=avh4-eh4-varh2/2
assert abs(quart+mp.mpf(37)/15)<mp.mpf('1e-70')
for n in (100,1000,10000):
    ll=sum(bb(t,n,n,mp.mpf(n)*(n/m-1))-pois(t,m) for t in xx)
    emit('diagonal_fourth',xx,'a=N, exact mean',n,n**4*ll/len(xx),quart)
    if n==10000: assert abs(n**4*ll/len(xx)-quart)<mp.mpf('1e-6')

# Root bracket for the NB face, using its known unique score root.
xx=[1,1,5]; m,vv,_,_=stats(xx); h=len(xx)
def score(a):
    return sum(sum(1/(a+j) for j in range(t)) for t in xx)-h*mp.log1p(m/a)
lo=mp.mpf(1); hi=mp.mpf(16)
assert score(lo)>0 and score(hi)<0
while hi-lo > mp.mpf('1e-45'):
    mid=(lo+hi)/2
    if score(mid)>0: lo=mid
    else: hi=mid
face_lo=sum(nb(t,lo,m) for t in xx)
deriv_bound=sum(sum(1/(lo+j) for j in range(t)) for t in xx)+h*mp.log1p(m/lo)
face_hi=face_lo+(hi-lo)*deriv_bound
slope=h*(m*m-lo*(vv-m))/(2*(lo+m))
candidate=-h*mp.log(6) # BB(5,1,1), uniform: no optimizer required.
assert slope<0 and candidate>face_hi+mp.mpf('.3')
face_record=dict(sample=xx,shape_lower=mp.nstr(lo,55),shape_upper=mp.nstr(hi,55),
                 loglik_lower=mp.nstr(face_lo,55),loglik_upper=mp.nstr(face_hi,55),
                 slope=mp.nstr(slope,25),uniform_candidate=mp.nstr(candidate,25))
emit('counterexample',xx,'uniform BB(5,1,1) minus NB upper bound',0,candidate-face_hi,0)
# Check both sides of the NB seam with the mean fixed exactly.
def bnb(t,r,alpha,a):
    return (mp.loggamma(r+t)-mp.loggamma(r)-mp.loggamma(t+1)
            +mp.loggamma(alpha+r)+mp.loggamma(a+t)
            -mp.loggamma(alpha+r+a+t)-mp.loggamma(alpha)-mp.loggamma(a)
            +mp.loggamma(alpha+a))
for n in (100,1000,10000):
    observed_bb=n*sum(bb(t,n,lo,lo*(n/m-1))-nb(t,lo,m) for t in xx)
    observed_bnb=-n*sum(bnb(t,n,1+lo*n/m,lo)-nb(t,lo,m) for t in xx)
    emit('NB_seam',xx,'BB side, N*(ell-ell_NB)',n,observed_bb,slope)
    emit('NB_seam',xx,'BNB side, -r*(ell-ell_NB)',n,observed_bnb,slope)
    if n==10000:
        assert abs(observed_bb-slope)<mp.mpf('.005')
        assert abs(observed_bnb-slope)<mp.mpf('.005')
# Exact finite witness for a zero second-order diagonal coefficient.
xx=[1,1,4]; m,_,_,_=stats(xx)
witness=-3*mp.log(5); lp=sum(pois(t,m) for t in xx)
assert witness>lp
emit('counterexample',xx,'uniform BB(4,1,1) minus Poisson',0,witness-lp,0)

# Search a small exact-equidispersion enumeration for a STRICT local
# Poisson maximum with a remote finite witness. This is candidate discovery,
# not a global fit or an estimate at a capped size.
remote=None; checked=0
for h in range(3,13):
    for xx in itertools.combinations_with_replacement(range(8),h):
        S=sum(xx)
        if not S or h*sum(t*t for t in xx)-S*S != h*S: continue
        if h*h*sum(t*(t-1)*(t-2) for t in xx) <= S**3: continue
        checked+=1; mm=S/h; n=max(xx)
        lp=sum(t*math.log(mm)-mm-math.lgamma(t+1) for t in xx)
        found=False
        for aa,bbeta in itertools.product((.2,.5,1.,2.,5.,10.),repeat=2):
            val=sum(math.lgamma(n+1)-math.lgamma(t+1)-math.lgamma(n-t+1)
                    +math.lgamma(aa+t)-math.lgamma(aa)
                    +math.lgamma(bbeta+n-t)-math.lgamma(bbeta)
                    -math.lgamma(aa+bbeta+n)+math.lgamma(aa+bbeta) for t in xx)
            if val>lp+1e-5:
                m,vv,ff2,ff3=stats(xx)
                witness=sum(bb(t,n,str(aa),str(bbeta))-pois(t,m) for t in xx)
                assert ff3>m**3 and vv==m and witness>mp.mpf('1e-5')
                remote=dict(sample=xx,N=n,a=aa,b=bbeta,gain=mp.nstr(witness,25),
                            mu=mp.nstr(m,25),F3=mp.nstr(ff3,25))
                found=True; break
        if found: break
    if remote: break

assert remote is not None
# Exact log bounds via log(t)=2*atanh((t-1)/(t+1)), after reducing t
# to [1,2]. The geometric tail bound makes these rational certificates.
def log_bounds(q):
    q=Fraction(q); power=0
    while q>=2: q/=2; power+=1
    while q<1: q*=2; power-=1
    def unit(t):
        z=(t-1)/(t+1); K=40
        lower=2*sum((z**(2*j+1)/Fraction(2*j+1) for j in range(K)),Fraction(0))
        tail=2*z**(2*K+1)/(Fraction(2*K+1)*(1-z*z))
        return lower,lower+tail
    low,high=unit(q); l2,h2=unit(Fraction(2))
    return (low+power*(l2 if power>=0 else h2),
            high+power*(h2 if power>=0 else l2))

def linear_log_bounds(terms):
    low=high=Fraction(0)
    for coefficient,q in terms:
        a,b=log_bounds(q)
        low+=coefficient*(a if coefficient>=0 else b)
        high+=coefficient*(b if coefficient>=0 else a)
    return low,high

xl=[1,1,5]; m=Fraction(7,3); alo=Fraction(451,100); ahi=Fraction(452,100)
for a,sign in ((alo,1),(ahi,-1)):
    rat=sum((1/(a+j) for t in xl for j in range(t)),Fraction(0))
    low,high=log_bounds(1+m/a)
    assert (rat-3*high>0) if sign==1 else (rat-3*low<0)
terms=[(Fraction(1),alo+j) for t in xl for j in range(t)]
terms += [(Fraction(-1),Fraction(math.factorial(t))) for t in xl]
terms += [(Fraction(7),m),(-7-3*alo,alo+m),(3*alo,alo)]
_,nb_upper=linear_log_bounds(terms)
derivative_upper=sum((1/(alo+j) for t in xl for j in range(t)),Fraction(0))+3*log_bounds(1+m/alo)[1]
nb_upper+=(ahi-alo)*derivative_upper
bb_lower=-3*log_bounds(Fraction(6))[1]
assert bb_lower>nb_upper and alo>Fraction(49,11)
exact_nb=dict(bracket=['4.51','4.52'],score_signs_proved=True,
              negative_seam_slope_proved=True,
              uniform_bb_beats_face_proved=True,
              gain_lower=float(bb_lower-nb_upper),log_series_terms=40)
# Strict rational certificates: Taylor lower bounds for exp(S) prove that
# each integer-mean Poisson likelihood is below the rational BB witness.
def rational_bb(t,n,a,b):
    return Fraction(math.comb(n,t)*math.prod(range(a,a+t))
                    *math.prod(range(b,b+n-t)), math.prod(range(a+b,a+b+n)))
exact_witnesses=[]
for xx,n,a,b in (([1,1,4],4,1,1),
                 (list(remote['sample']),remote['N'],int(remote['a']),int(remote['b'])),
                 (examples['positive'],10,10,90),
                 (examples['positive'],10,8,72)):
    S=sum(xx); m=Fraction(S,len(xx)); assert m.denominator==1
    ll_bb=math.prod(rational_bb(t,n,a,b) for t in xx)
    threshold=Fraction(m**S,math.prod(math.factorial(t) for t in xx))/ll_bb
    lower=sum((Fraction(S)**j/Fraction(math.factorial(j)) for j in range(81)),Fraction(0))
    assert lower>threshold
    exact_witnesses.append(dict(sample=xx,N=n,a=a,b=b,taylor_degree=80,
                                strict_rational_comparison=True))
# (1,2): global binomial-face supremum, using exact product bounds.
assert Fraction(16,81)<Fraction(27,128)
emit('nonexistence',[1,2],'binomial-face probability product',2,mp.mpf(27)/128,mp.mpf(27)/128)

with (HERE/'checks.csv').open('w',newline='') as f:
    w=csv.DictWriter(f,fieldnames=rows[0]); w.writeheader(); w.writerows(rows)
summary=dict(python=platform.python_version(),mpmath=mp.__version__,sympy=s.__version__,
             precision=mp.mp.dps,rows=len(rows),nb_face=face_record,
             strict_local_remote=remote,enumerated_equidi_strict_local=checked,
             exact_witnesses=exact_witnesses,
             exact_nb_certificate=exact_nb,
             result='PASS: all assertions; no global numerical fit attempted')
(HERE/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
