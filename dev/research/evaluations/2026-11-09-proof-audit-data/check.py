#!/usr/bin/env python3
"""Independent adversarial formula checks. Finite numerical evidence, not proof.

Run from the worktree root with /tmp/umvl-cpu/run python3 <this-file>.
Only this directory's results.json is written. All decimal literals are exact
decimal inputs; random.Random(165) supplies reproducible inputs and exponents.
"""
import hashlib
import json
import random
import os
from collections import defaultdict
from pathlib import Path
import mpmath as mp

mp.mp.dps = 120
rng = random.Random(165)
HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
counts = defaultdict(int)
minimum = {}
TINY = mp.mpf('1e-70')
ITERLIM = int(os.environ.get('UMVL_AUDIT_ITERLIM', '512'))
assert ITERLIM > 0
SOLVER_BUDGET = min(ITERLIM, 100000)
COVER_BUDGET = 128 * SOLVER_BUDGET


def M(x):
    return mp.mpf(str(x))


def ge(tag, lhs, rhs=0, strict=False):
    slack = lhs-rhs
    scale = 1+abs(lhs)+abs(rhs)
    assert slack > 0 if strict else slack >= -TINY*scale, (tag, str(slack))
    counts[tag] += 1
    minimum[tag] = min(minimum.get(tag, mp.inf), slack/scale)


def eq(tag, lhs, rhs):
    assert abs(lhs-rhs) <= TINY*(1+abs(lhs)+abs(rhs)), (tag, str(lhs-rhs))
    counts[tag] += 1


def logrand(lo=-8, hi=8):
    return mp.power(10, M(rng.randrange(lo*100, hi*100+1))/100)


def mean(x):
    return mp.fsum(x)/len(x)


def pig():
    for mu in [M('1e-20'),M(1),M('1e20')]:
        eq('pig.zero_law',mp.exp(-mu*(1-M(1))),M(1))
        for w in [M('.1'),M('.5'),M('.9')]:
            ge('pig.zero_sample_increasing',mu,0,True)
            ge('pig.zero_sample_below_maximum',0,-mu*(1-w),True)
    for k in [2, 3, 4, 10, 50, 100]:
        c = k*(k-1)
        for z in [mp.power(10,j) for j in [-20,-8,-2,0,2,8,20]] + [logrand() for _ in range(15)]:
            terms = [mp.factorial(k-1+j)/mp.factorial(j)/mp.factorial(k-1-j)/(2*z)**j for j in range(k)]
            total = mp.fsum(terms)
            E = mp.fsum(j*t for j,t in enumerate(terms))/total
            V = mp.fsum((j-E)**2*t for j,t in enumerate(terms))/total
            eq('pig.ODE', V, c-(2*z+1)*E-E*E)
            b = E*(k*k-E*E)/(k*k+E*E)
            ge('pig.variance_barrier', b, V, True)
            bp = mp.diff(lambda e:e*(k*k-e*e)/(k*k+e*e), E)
            # Differentiate V=c-(2z+1)E-E², then substitute V=b.
            cross = -2*z*E+(2*z+1+2*E+bp)*b
            zcross = (c-E-E*E-b)/(2*E)
            cross = -2*zcross*E+(2*zcross+1+2*E+bp)*b
            expected = 2*E*k*(E+k)**2*(E**3+E**2*k-E*k*k+k**3)/(E*E+k*k)**3
            eq('pig.crossing_identity', cross, expected)
            ge('pig.crossing_sign', expected, 0, True)
            for w in [M('1e-12'), M('.01'), M('.5'), 1-M('1e-12'), E/k]:
                r = E/k
                curvature = -4*k*w+2*E*(3+w*w)+(V-E)*(1+w*w)**2/w**2
                bound = 2*(r-w)**2*(r*w*w-r-2*w)/(w*w*(1+r*r))
                ge('pig.curvature_barrier', bound, curvature/k, True)
                ge('pig.factor_sign', 0, bound)
        # Derivatives independently from the finite mass polynomial.
        for w in [M('.0001'), M('.3'), M('.99')]:
            mu = M('1.7')
            def masslog(v):
                zz = mu*(1-v*v)/(2*v)
                Q = mp.fsum(mp.factorial(k-1+j)/mp.factorial(j)/mp.factorial(k-1-j)/(2*zz)**j for j in range(k))
                return -mu*(1-v)+k*mp.log(mu*(1-v)/(1+v))+mp.log(Q)-mp.loggamma(k+1)
            ge('pig.direct_curvature', 0, mp.diff(masslog,w,2), True)
    # Joint reduction: t=lambda/mu, lambda=(z²-t²)/2.
    for _ in range(60):
        m,z = logrand(-3,3),logrand(-3,3)
        t = z*z/(mp.sqrt(m*m+z*z)+m)
        lam = (z*z-t*t)/2
        eq('pig.mean_reduction', lam/t,m)
        ge('pig.fixed_z_curvature', 0, -2*m*(z*z+t*t)/(z*z-t*t)**2,True)


def frechet():
    samples = [[0],[0,0],[0,0,1,2],[0,0,0,1],[0,1,2,3], [0,M('1e-20'),1,M('1e20')]]
    samples += [[M(rng.randrange(6)) for _ in range(rng.randrange(2,10))] for _ in range(70)]
    for xs in samples:
        xs=[M(x) for x in xs]; g=[x-min(xs) for x in xs]; n=len(g); k=g.count(0)
        shapes=[M('1e-8'),M('.01'),M('.3'),M(1),M(5),M('1e4'),M('1e8')]
        if n>k: shapes += [M(k)/(n-k), M(k)/(n-k)*(1+M('1e-15')),M(k)/(n-k)*(1-M('1e-15'))]
        for a in shapes:
            for e in [M('1e-25'),M('.01'),M(1),M('1e25')]:
                def S(p): return mp.fsum((v+e)**(-p) for v in g)
                def E(p): return S(p+1)/S(p)
                R=S(a+1)/(S(a)*S(1))
                qi=[1/(d+e) for d in g]
                wi=[v**a/S(a) for v in qi]
                covariance=mp.fsum(wi[i]*wi[j]*(qi[i]-qi[j])*(mp.log(qi[i])-mp.log(qi[j])) for i in range(n) for j in range(i))
                ge('frechet.weighted_covariance',covariance,0,n>k)
                deriv=-(a+1)*E(a+1)+a*E(a)+E(1)
                ge('frechet.ratio_derivative',0,deriv, n>k)
                ge('frechet.ratio_upper', M(1)/k,R)
                ge('frechet.ratio_lower', R,M(1)/n)
                # Profile derivative is S1*(n*a*R-(a+1)).
                def profile(v):
                    sa=mp.fsum((d+v)**(-a) for d in g)
                    return n*mp.log(a)+n*mp.log(n/sa)-(a+1)*mp.fsum(mp.log(d+v) for d in g)-n
                eq('frechet.profile_score',mp.diff(profile,e),S(1)*(n*a*R-a-1))
                ge('frechet.held_scale_ratio_derivative',0,-(a+1)*E(a+1)+E(1),True)
            if n>k and a==M(k)/(n-k):
                limit=n*mp.log(n*a)-n-n*mp.log(k)-(a+1)*mp.fsum(mp.log(d) for d in g if d)
                ge('frechet.equality_supremum',limit,profile(M('1e-25')),True)


def nb():
    for r,p in [(M(0),M('.1')),(M(0),M('.5')),(M('.1'),M(1)),(M(100),M(1))]:
        eq('nb.zero_law',p**r,M(1))
        for x in [1,10]:
            eq('nb.zero_law_positive_count',mp.rf(r,x)*p**r*(1-p)**x/mp.factorial(x),M(0))
    samples=[[0,2],[0,1],[0,0,1],[0,0,0,1],[0,0,6],[1],[2,2],[0,1,2,3]]
    samples += [[rng.randrange(15) for _ in range(rng.randrange(1,12))] for _ in range(90)]
    for k in [2,10,100]:
        samples += [[k*k+d-k,k*k+d+k] for d in [-1,0,1]]
    for xs in samples:
        if not sum(xs):continue
        n=len(xs);m=M(sum(xs))/n;v=mean([(x-m)**2 for x in xs]);last=None
        for r in [M('1e-12'),M('.01'),M(1),M(100),M('1e12')]:
            fp=n*mp.log1p(m/r)-mp.fsum(mp.digamma(r+x)-mp.digamma(r) for x in xs)
            bp=(mp.log1p(m/r)-m/(r+m))/m
            bpp=-m/(r*(r+m)**2)
            fpp=-n*m/(r*(r+m))+mp.fsum(mp.polygamma(1,r)-mp.polygamma(1,r+x) for x in xs)
            h=fpp/bpp;G=fp/bp
            ge('nb.b_increasing',bp,0,True)
            ge('nb.b_concave',0,bpp,True)
            ge('nb.weighted_average',G,h,True)
            ge('nb.limit_bound',n*(v-m)/m,G,True)
            if last is not None:ge('nb.G_increasing',G,last,True)
            last=G
            if v<=m:ge('nb.poisson_score',0,fp,True)
            for q in set(xs):
                if q==0:continue
                # Keep explicitly expanded sums modest; high-count checks above use polygamma.
                if q>30:continue
                d=min(q-1,int(mp.floor(m)))
                eq('nb.telescoping',r*mp.fsum(1/(r+j)**2 for j in range(d+1)),(d+1)/(r+d)+mp.fsum(M(j)/((r+j)**2*(r+j-1)) for j in range(1,d+1)))
                T=lambda t:t*(t+m)**2*mp.fsum(1/(t+j)**2 for j in range(q))
                ge('nb.T_strict_convexity',mp.diff(T,r,2),0,True)
                ge('nb.first_factor_curvature',2*(m-d)**2/(r+d)**3,0)
                for j in range(1,d+1):
                    ge('nb.middle_log_curvature',-2/(r+m)**2+2/(r+j)**2+1/(r+j-1)**2,0,True)
                for j in range(d+1,q):
                    formula=(2*(m-j)*(m-3*j)*r+4*m*j*(j-m))/(r+j)**4
                    eq('nb.final_factor_identity',mp.diff(lambda t:t*(t+m)**2/(t+j)**2,r,2),formula)
                    ge('nb.final_factor_sign',formula,0,True)
    # Independent evaluation of AEE equation (4), not just the quoted boundary value.
    def u(z,m,a):return ((2*z+1)*mp.log1p(m*z)-m*z)/(2*z*z)-a/(2*(1+z))
    eq('aee.boundary_z1',u(M(1),M(0),M('.25')),-M(1)/16)
    eq('aee.boundary_z2',u(M(2),M(0),M('.25')),-M(1)/24)
    eq('aee.omitted_difference',u(M(2),M(0),M('.25'))-u(M(1),M(0),M('.25')),M(1)/48)
    for m0 in [M('1e-20'),M('.1'),M('.5'),M(1),2-M('1e-20')]:
        for z1 in [M('1e-12'),M('.1'),M(1),M('1e12')]:
            z2=2*z1
            mixed=mp.diff(lambda zz:mp.diff(lambda mm:u(zz,mm,M('.25')),m0),z1)
            eq('aee.mixed_derivative',mixed,-(2-m0)*m0/(2*(1+m0*z1)**2))
            ge('aee.valid_integrated_step',u(z2,0,M('.25'))-u(z1,0,M('.25')),u(z2,m0,M('.25'))-u(z1,m0,M('.25')),True)


def sged():
    R=mp.findroot(lambda r:mp.log(r)-2*(1-1/r),(4,6),maxsteps=SOLVER_BUDGET)
    K=2*mp.sqrt(R-1)/R;xi=mp.sqrt(1+K*K/6)
    # Derive the cusp coefficient from the standardized GED moments.
    nu=M('.5')
    lam=2**(-1/nu)*mp.sqrt(mp.gamma(1/nu)/mp.gamma(3/nu))
    m1=mp.gamma(2/nu)/mp.sqrt(mp.gamma(1/nu)*mp.gamma(3/nu))
    d=m1*(xi-1/xi)
    momentK=abs(d/lam)**nu*xi**nu/2
    eq('sged.moment_to_K',momentK,K)
    su=mp.sqrt((1-m1*m1)*(xi*xi+xi**-2)+2*m1*m1-1)
    sd_cusp=su/d;sd_other=su/(d*R)
    # Density ratio in public sd coordinates; normalizer cancels.
    z=-su/sd_other+d
    eq('sged.public_density_tie',mp.log(sd_cusp/sd_other)-abs(z/(lam*xi**mp.sign(z)))**nu/2,0)
    eq('sged.tie',-mp.log(R)+K*mp.sqrt(R-1),0)
    eq('sged.stationarity',-1/R+K/(2*mp.sqrt(R-1)),0)
    rmax=1+1/(R-1)
    ge('sged.separated_stationary',R,rmax,True)
    f=lambda r:-mp.log(r)+K*mp.sqrt(r-1)
    ge('sged.first_is_maximum',0,mp.diff(f,rmax,2),True)
    ge('sged.second_is_minimum',mp.diff(f,R,2),0,True)
    for r in [1+M('1e-30'),rmax,R, M('1e30')]+[1+logrand(-15,15) for _ in range(200)]:
        ge('sged.global_nonnegative',f(r),0)
    for _ in range(200):
        nu=M(rng.randrange(1,100))/100;k=logrand(-4,4);skew=logrand(-3,3);r=2+logrand(-6,6)
        b=M(rng.choice([-1,1]));d=M(rng.choice([-1,1]))
        penalty=k*skew**(-nu*mp.sign(d*(1+b*r)))*abs(1+b*r)**nu
        C=k*mp.exp(-abs(nu*mp.log(skew)))/2**nu
        ge('sged.tail_envelope',penalty,C*r**nu)
        l=M('.1');h=M('.8');v=l+(h-l)*M(rng.randrange(101))/100
        P=lambda t:k*skew**(-nu*d)*abs(1+t)**nu
        chord=P(l)+(P(h)-P(l))*(v-l)/(h-l)
        ge('sged.concave_chord',P(v),chord)
        ge('sged.left_cell_decreasing',0,-1/v-K/(2*xi*mp.sqrt(1-v)),True)
        ge('sged.derivative_lower',mp.diff(P,v)-1/v,mp.diff(P,h)-1/l)
        ge('sged.derivative_upper',mp.diff(P,l)-1/h,mp.diff(P,v)-1/v)
        # A negative b knot at one: test both adjacent sign cells.
        theta=(v-l)/(h-l)
        for left,right in [(M('.1'),M('.8')),(M('1.1'),M(3))]:
            val=left+(right-left)*theta
            Pc=lambda rr:k*skew**(-nu*mp.sign(d*(1-rr)))*abs(1-rr)**nu
            slope=(Pc(right)-Pc(left))/(right-left)
            chordc=lambda rr:Pc(left)+slope*(rr-left)
            minimum_r=max(left,min(right,1/slope)) if slope>0 else right
            ge('sged.both_sign_chords',Pc(val),chordc(val))
            ge('sged.chord_minimum',-mp.log(val)+Pc(val),-mp.log(minimum_r)+chordc(minimum_r))
    return {'R':str(R),'K':str(K),'xi':str(xi),'other_stationary_r':str(rmax),'public_sd_maxima':[str(sd_cusp),str(sd_other)]}


def profile(x,p,q):
    ys=list(map(mp.log,x));lo=mp.log(q/p)+min(ys);hi=mp.log(q/p)+max(ys)
    # A global lower bound on the absolute scale-score derivative over the
    # initial root bracket. This converts residual into a root error bound,
    # including flat cases, rather than treating a small residual as a root.
    ulo=[1/(1+mp.exp(lo-y)) for y in ys]
    uhi=[1/(1+mp.exp(hi-y)) for y in ys]
    derivative_floor=(p+q)*mean([min(v*(1-v),w*(1-w)) for v,w in zip(ulo,uhi)])
    assert derivative_floor>0
    t=(lo+hi)/2
    for _ in range(SOLVER_BUDGET):
        u=[1/(1+mp.exp(t-y)) for y in ys]; Q=mean([v*(1-v) for v in u]);score=-p+(p+q)*mean(u)
        root_error=abs(score)/derivative_floor
        if abs(score)<M('1e-85')*min(p,q) and root_error<M('1e-75'):break
        if score>0:lo=t
        else:hi=t
        new=t+score/((p+q)*Q)
        t=new if lo<new<hi else (lo+hi)/2
    else:raise AssertionError('scale root budget exhausted')
    ge('betapr.scale_root_error_bound',M('1e-75'),root_error,True)
    f=mean([(p-1)*y-p*t-(p+q)*mp.log1p(mp.exp(y-t)) for y in ys])-mp.loggamma(p)-mp.loggamma(q)+mp.loggamma(p+q)
    D=mp.digamma(p+q)-mp.digamma(q)-mean([mp.log1p(mp.exp(y-t)) for y in ys])
    return f,D,t


def betapr():
    for _ in range(160):
        x=[logrand(-8,8) for _ in range(rng.randrange(2,9))];ys=list(map(mp.log,x));n=len(x)
        p,q=logrand(-5,6),logrand(-5,6);t=mp.log(logrand(-12,12));h=p*q/(p+q)
        ll=-n*(mp.loggamma(p)+mp.loggamma(q)-mp.loggamma(p+q))-mp.fsum(p*mp.log1p(mp.exp(t-y))+q*mp.log1p(mp.exp(y-t))+y for y in ys)
        ge('betapr.small_h',n*mp.log(h)-sum(ys),ll)
        modeheight=p*mp.log(p)+q*mp.log(q)-(p+q)*mp.log(p+q)-mp.loggamma(p)-mp.loggamma(q)+mp.loggamma(p+q)
        ge('betapr.mode_height',mp.log(h),modeheight)
        ge('betapr.Stirling_height',mp.log(h/(2*mp.pi))/2+1/(48*h),modeheight)
        d=logrand(-8,3);rho=d-1+mp.exp(-d)
        ge('betapr.rho_rational',rho,d*d/(2*(1+d)))
        ge('betapr.exp_tangent',mp.expm1(d),d)
        ge('betapr.rho_log_step',rho,d-mp.log1p(d))
        ge('betapr.rho_rational_step',d-mp.log1p(d),d*d/(2*(1+d)))
        for sign in [-1,1]:
            z=sign*d
            ratio=mp.exp(z)*(p+q)**2/(q+p*mp.exp(z))**2
            ge('betapr.curvature_loss',ratio,mp.exp(-d))
            loss=-p*z+(p+q)*mp.log((q+p*mp.exp(z))/(p+q))
            ge('betapr.integrated_loss',loss,h*rho)
        sy=sorted(ys);J=mp.fsum((sy[-i-1]-sy[i])**2/(4+2*(sy[-i-1]-sy[i])) for i in range(n//2))
        U=n*mp.log(h/(2*mp.pi))/2+n/(48*h)-h*J-sum(ys)
        ge('betapr.large_h',U,ll)
        H=max(M(1),n/(2*J));ge('betapr.U_decreasing',0,n/(2*H)-n/(48*H*H)-J,True)
        ge('betapr.trigamma_bound',mp.polygamma(1,p),1/p+1/(2*p*p),True)
        ge('betapr.trigamma_small_argument',mp.polygamma(1,p),1/(p*p),True)
        ge('betapr.log_variance_positive',mp.polygamma(1,p)+mp.polygamma(1,q),0,True)
        zz=logrand(-12,12)
        ge('betapr.log_quadratic',mp.log1p(zz),zz-zz*zz/2)
        kernel=mp.coth(zz/2)/2-1/zz
        ge('betapr.Binet_kernel_positive',kernel,0,True)
        ge('betapr.Binet_kernel_upper',zz/12,kernel,True)
        ge('betapr.digamma_bound',mp.log(p)-1/(2*p),mp.digamma(p),True)
        ge('betapr.gamma_unsigned_increment',p*mp.log(p+q),mp.loggamma(p+q)-mp.loggamma(q))
    for _ in range(80):
        x=[logrand(-3,3) for _ in range(rng.randrange(2,9))];x=[v/mean(x) for v in x]
        a=logrand(-3,2);Mmax=max(x);V=mean([v*v for v in x]);q=2*a*Mmax*(1+logrand(-3,6))
        f,D,t=profile(x,a,q);r=q/mp.exp(t)
        gamma=a*mp.log(a)-mp.loggamma(a)-a+(a-1)*mean(list(map(mp.log,x)))
        A=a*(a*(V-1)-1)/2
        C=a*a/4+2*a**3*Mmax*(1+2*V)+2*a**3*V
        ge('betapr.r_lower',r,a)
        ge('betapr.r_upper',2*a,r)
        ge('betapr.r_displacement',2*a*a*Mmax/q,r-a)
        ge('betapr.unsigned_tail',gamma+a*a*(1+2*V)/q,f)
        ge('betapr.signed_tail',gamma+A/q+C/q**2,f)
        incr=mp.loggamma(q+a)-mp.loggamma(q)-a*mp.log(q)
        ge('betapr.gamma_increment',a*(a-1)/(2*q)+a*a/(4*q*q),incr)
        L=logrand(-5,-1);b=L*M('.37');f0,D0,t0=profile(x,a,b)
        low=a/(a+L)-L*mean([mp.log1p(a*v/(L*min(x))) for v in x])
        ge('betapr.lower_exterior_score',b*D0,low)
        # Profile derivative and curvature at a representative shape.
        q0=logrand(-2,2);f0,D0,t0=profile(x,a,q0)
        ge('betapr.scale_bounds_low',mp.exp(t0),q0*min(x)/a)
        ge('betapr.scale_bounds_high',q0*max(x)/a,mp.exp(t0))
        l=q0/M('1.1');u=q0*M('1.1');fl,dl,tl=profile(x,a,l);fu,du,tu=profile(x,a,u)
        ge('betapr.scale_monotone',tu,tl,True)
        lower=mp.digamma(a+u)-mp.digamma(u)-mean([mp.log1p(v/mp.exp(tl)) for v in x])
        upper=mp.digamma(a+l)-mp.digamma(l)-mean([mp.log1p(v/mp.exp(tu)) for v in x])
        ge('betapr.profile_score_lower',D0,lower)
        ge('betapr.profile_score_upper',upper,D0)
        ys=list(map(mp.log,x))
        fun=lambda pp,qq,tt:mean([(pp-1)*y-pp*tt-(pp+qq)*mp.log1p(mp.exp(y-tt)) for y in ys])-mp.loggamma(pp)-mp.loggamma(qq)+mp.loggamma(pp+qq)
        uu0=[1/(1+mp.exp(t0-y)) for y in ys];QQ=mean([v*(1-v) for v in uu0])
        htt=mp.diff(lambda tt:fun(a,q0,tt),t0,2)
        hp=mp.diff(lambda pp:mp.diff(lambda tt:fun(pp,q0,tt),t0),a)
        hq=mp.diff(lambda qq:mp.diff(lambda tt:fun(a,qq,tt),t0),q0)
        endpoint_lo=[1/(1+mp.exp(tl-y)) for y in ys]
        endpoint_hi=[1/(1+mp.exp(tu-y)) for y in ys]
        qlower=mean([min(v*(1-v),w*(1-w)) for v,w in zip(endpoint_lo,endpoint_hi)])
        qupper=mean([M('.25') if w<=M('.5')<=v else max(v*(1-v),w*(1-w)) for v,w in zip(endpoint_lo,endpoint_hi)])
        ge('betapr.Q_lower',QQ,qlower)
        ge('betapr.Q_upper',qupper,QQ)
        eq('betapr.Htt',htt,-(a+q0)*QQ)
        eq('betapr.Hpt',hp,-q0/(a+q0))
        eq('betapr.Hqt',hq,a/(a+q0))
        eq('betapr.profile_Hpp',mp.diff(lambda pp:fun(pp,q0,t0),a,2)-hp*hp/htt,mp.polygamma(1,a+q0)-mp.polygamma(1,a)+q0*q0/((a+q0)**3*QQ))
        eq('betapr.profile_Hqq',mp.diff(lambda qq:fun(a,qq,t0),q0,2)-hq*hq/htt,mp.polygamma(1,a+q0)-mp.polygamma(1,q0)+a*a/((a+q0)**3*QQ))
        eq('betapr.profile_Hpq',mp.polygamma(1,a+q0)-hp*hq/htt,mp.polygamma(1,a+q0)-a*q0/((a+q0)**3*QQ))
        ge('betapr.trigamma_increment_monotone',mp.polygamma(1,a+u)-mp.polygamma(1,u),mp.polygamma(1,a+l)-mp.polygamma(1,l),True)
        # Gamma curvature and the completed-square signed uniform bound.
        aa=logrand(-2,2);uu=max(aa,a);kappa=1/(2*uu*uu)
        G=lambda v:v*mp.log(v)-mp.loggamma(v)-v+(v-1)*mean(list(map(mp.log,x)))
        ge('betapr.gamma_strong_concavity',G(aa)+mp.diff(G,aa)*(a-aa)-kappa*(a-aa)**2/2,G(a))
        Ap=lambda v:v*(v*(V-1)-1)/2
        BB=M('.5')+uu*(V-1)
        ge('betapr.A_lipschitz',Ap(aa)+BB*abs(a-aa),Ap(a))
        displacement=abs(a-aa)
        ge('betapr.complete_square',BB*BB*uu*uu/q**2,-kappa*displacement**2/2+BB*displacement/q)
    # Independent compactness proof for tie path and endpoint losing-side covers.
    case=json.loads((ROOT/'dev/research/evaluations/2026-11-07-betapr-prof-data/counterexample-refined.json').read_text(),parse_float=str)
    a=M(case['a']);base=list(map(M,case['x']));cut=M('.12');lower=M('.000001');upper=M('1e20')
    rows=[]
    for tval,losing in [(M('5.6463'),'left'),(M('5.6466'),'right')]:
        x=base[:];x[-1]*=mp.exp(tval);n=len(x)
        roots=[]
        for start in [M('.0835'),M('.1688')]:
            q=mp.findroot(lambda b:profile(x,a,b)[1],(start*M('.999'),start*M('1.001')),solver='secant',maxsteps=SOLVER_BUDGET)
            f,D,ls=profile(x,a,q);roots.append((q,f,ls))
        winner=roots[1 if losing=='left' else 0];inc=winner[1]-TINY
        cache={}
        def at(q):
            if q not in cache:cache[q]=profile(x,a,q)
            return cache[q]
        # Small-q derivative positive uniformly, using worst normalized extrema.
        z=[v/mean(x) for v in x]
        small=a/(a+lower)-lower*mean([mp.log1p(a*v/(lower*min(z))) for v in z])
        ge('betapr.tie_lower_exterior',small,0,True)
        # Unsigned tail below incumbent, including all q>=upper.
        gam=a*mp.log(a/mean(x))-mp.loggamma(a)-a+(a-1)*mean(list(map(mp.log,x)))
        tail=gam+a*a*(1+2*mean([v*v for v in z]))/upper
        ge('betapr.tie_upper_exterior',inc,tail,True)
        stack=[(lower,cut)] if losing=='left' else [(cut,upper)]
        cells=0;gap=mp.inf
        while stack:
            l,u=stack.pop();fl,_,tl=at(l);fu,_,tu=at(u)
            dlo=mp.digamma(a+u)-mp.digamma(u)-mean([mp.log1p(v/mp.exp(tl)) for v in x])
            dhi=mp.digamma(a+l)-mp.digamma(l)-mean([mp.log1p(v/mp.exp(tu)) for v in x])
            # |t-t*|<delta gives |D-D_exact|<=delta. Since |f_tt|<=
            # (a+q)/4, the value loss is at most (a+q)*delta²/8.
            # TINY is an additional practical arithmetic allowance; the
            # accepted separation below is a much larger 1e-12 per datum.
            delta=M('1e-75')
            dlo-=delta+TINY;dhi+=delta+TINY
            bound=min(fl+(a+l)*delta**2/8+TINY+max(M(0),dhi)*(u-l),
                      fu+(a+u)*delta**2/8+TINY+max(M(0),-dlo)*(u-l))
            cells+=1
            assert cells<=COVER_BUDGET,'cover budget exhausted'
            if bound<inc-M('1e-12'):
                gap=min(gap,inc-bound)
            else:
                mid=mp.sqrt(l*u);stack.extend([(l,mid),(mid,u)])
        counts['betapr.tie_losing_cover_cells']+=cells
        rows.append({'t':str(tval),'losing_side':losing,'cover_cells':cells,'profile_points':len(cache),'minimum_per_datum_gap':str(gap),'stationary':[[str(v) for v in r] for r in roots]})
    center=M('5.646438395028742');x=base[:];x[-1]*=mp.exp(center)
    q=M('.08352762816269817');f,_,ls=profile(x,a,q);barrier=profile(x,a,cut)[0]+TINY+(a+cut)*M('1e-150')/8
    gamma=a*mp.log(a/mean(x))-mp.loggamma(a)-a+(a-1)*mean(list(map(mp.log,x)))
    radius=max(center-M('5.6463'),M('5.6466')-center)
    bg=len(x)*(f-barrier)-(2+q+cut)*radius
    fg=len(x)*(f-gamma)-(2+q+len(x)*a)*radius
    ge('betapr.tie_barrier_gap',bg,M('.17569'),True)
    ge('betapr.tie_face_gap',fg,M('17.33799'),True)
    return {'endpoint_covers':rows,'uniform_barrier_gap':str(bg),'uniform_face_gap':str(fg),'uniform_q_range':[str(lower),str(upper)]}


def bbinom():
    fixtures=[(2,[0,1,2]),(2,[0,2]),(2,[0,1,1,2]),(3,[0,1,2,3]),(3,[0,1,1,2,2,3])]
    fixtures += [(N,[rng.randrange(N+1) for _ in range(rng.randrange(2,20))]) for N in [2,3,5,10,20] for _ in range(20)]
    for N,x in fixtures:
        h=len(x);S=sum(x)
        if S==0 or S==h*N:continue
        p0=M(S)/(h*N);Q=sum(v*v for v in x)
        A=[sum(v>j for v in x) for j in range(N)];C=[sum(v<N-j for v in x) for j in range(N)]
        def ell(p,s):return mp.fsum(A[j]*mp.log(p+j*s)+C[j]*mp.log(1-p+j*s)-h*mp.log1p(j*s) for j in range(N))
        g0=M(N*h*Q-(N-1)*S*S-N*h*S)/(2*h*N*p0*(1-p0));B=M(h*(N-1)*N*(2*N-1))/6;F=ell(p0,0)
        eq('bbinom.g0',g0,mp.diff(lambda s:ell(p0,s),M(0)))
        for s in [M('1e-20'),M('.0001'),M('.1'),M(10)]:
            for p in [M('1e-15'),p0,M('.5'),1-M('1e-15')]:
                ge('bbinom.uniform_face_bound',g0*s+B*s*s/2,ell(p,s)-F)
        if g0<0:
            s=-g0/B
            ge('bbinom.negative_strip',0,g0*s+B*s*s/2,True)
        l=p0/2;u=(1+p0)/2;t=M('.02');s=t*M('.37');p=(l+u)/2
        amin=[M(A[j])/(u+j*t)**2 for j in range(N)];amax=[M(A[j])/l**2 for j in range(N)]
        cmin=[M(C[j])/(1-l+j*t)**2 for j in range(N)];cmax=[M(C[j])/(1-u)**2 for j in range(N)]
        MM=sum(amin)+sum(cmin);J=sum(j*j*(h-amin[j]-cmin[j]) for j in range(N))
        km=sum(j*(cmin[j]-amax[j]) for j in range(N));kp=sum(j*(cmax[j]-amin[j]) for j in range(N))
        pp=mp.diff(lambda v:ell(v,s),p,2);ps=mp.diff(lambda v:mp.diff(lambda w:ell(v,w),s),p);ss=mp.diff(lambda v:ell(p,v),s,2)
        ge('bbinom.M_bound',-pp,MM)
        ge('bbinom.J_bound',J,ss)
        ge('bbinom.K_lower',ps,km)
        ge('bbinom.K_upper',kp,ps)
        ge('bbinom.Schur_bound',J+max(abs(km),abs(kp))**2/MM,ss+ps*ps/(-pp))
        score=lambda p,s:mp.diff(lambda v:ell(v,s),p)
        ge('bbinom.rectangle_lower',score(l,s),sum(M(A[j])/(l+j*t)-M(C[j])/(1-l) for j in range(N)))
        ge('bbinom.rectangle_upper',sum(M(A[j])/u-M(C[j])/(1-u+j*t) for j in range(N)),score(u,s))
        left=M('.01');right=M('.3');mid=(left+right)/2;pm=p0
        vs=sum(j*(M(A[j])/(pm+j*mid)+M(C[j])/(1-pm+j*mid)) for j in range(N))
        D=lambda v:h*sum(mp.log1p(j*v) for j in range(N))
        ub=ell(pm,mid)+abs(score(pm,mid))+max(vs*(v-mid)-D(v)+D(mid) for v in [left,right])
        for ptest in [M('.0001'),p0,M('.9999')]:
            ge('bbinom.tangent_chord',ub,ell(ptest,left+(right-left)*M('.37')))
    # Zero-score table: a strictly negative uniform curvature strip.
    N=2;h=4;A=[3,1];C=[3,1];l=M('.499');u=M('.501');t=M('.0001')
    amin=[M(A[j])/(u+j*t)**2 for j in range(N)]
    cmin=[M(C[j])/(1-l+j*t)**2 for j in range(N)]
    amax=[M(A[j])/l**2 for j in range(N)];cmax=[M(C[j])/(1-u)**2 for j in range(N)]
    MM=sum(amin)+sum(cmin);J=sum(j*j*(h-amin[j]-cmin[j]) for j in range(N))
    km=sum(j*(cmin[j]-amax[j]) for j in range(N));kp=sum(j*(cmax[j]-amin[j]) for j in range(N))
    curvature=J+max(abs(km),abs(kp))**2/MM
    ge('bbinom.zero_score_negative_curvature',0,curvature,True)
    ge('bbinom.zero_score_rectangle_left',sum(M(A[j])/(l+j*t)-M(C[j])/(1-l) for j in range(N)),0,True)
    ge('bbinom.zero_score_rectangle_right',0,sum(M(A[j])/u-M(C[j])/(1-u+j*t) for j in range(N)),True)
    # Symmetry fixes the profiled p at 1/2 throughout this strip.
    ell=lambda s:sum(A[j]*mp.log(M('.5')+j*s)+C[j]*mp.log(M('.5')+j*s)-h*mp.log1p(j*s) for j in range(N))
    for s in [t/M(100),t/2,t]:
        ge('bbinom.zero_score_signed_strip',curvature*s*s/2,ell(s)-ell(M(0)))
    return {'fixture_count':len(fixtures)}


if __name__=='__main__':
    extra={}
    for name,fn in [('pig',pig),('frechet',frechet),('nb',nb),('sged',sged),('betapr',betapr),('bbinom',bbinom)]:
        print('Checking '+name,flush=True)
        extra[name]=fn()
    out={'result':'PASS','decimal_digits':mp.mp.dps,'seed':165,'iterlim':ITERLIM,'comparison_allowance':str(TINY),'mpmath':mp.__version__,'checks':dict(counts),'total_checks':sum(counts.values()),'recorded_comparisons':sum(counts.values())-counts['betapr.tie_losing_cover_cells'],'cover_cells':counts['betapr.tie_losing_cover_cells'],'minimum_scaled_slacks':{k:str(v) for k,v in minimum.items()},'details':extra,'script_sha256':hashlib.sha256(Path(__file__).read_bytes()).hexdigest()}
    (HERE/'results.json').write_text(json.dumps(out,indent=2)+'\n')
    print(json.dumps({'result':'PASS','total_checks':out['total_checks'],'groups':len(counts)}))
