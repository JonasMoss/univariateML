"""High-precision research completion of finite intervals and normal tail.

Subdivisions are exhaustive. Roots are only accepted through roots.py's
root box AND uniform nuisance/curvature tube. Unclosed segments give budget,
not coefficients. This is deliberately independent of production dispatch.
"""
import heapq
import json
import sys
import time
from fractions import Fraction
from pathlib import Path
import mpmath as mp
import numpy as np
from scipy.optimize import minimize
import roots

HERE=Path(__file__).resolve().parent
OLD=roots.OLD
study=roots.study
mp.mp.dps=65

class BudgetError(RuntimeError):
    pass

def strings(v):
    if isinstance(v,mp.mpf):return str(v)
    if isinstance(v,dict):return {k:strings(q) for k,q in v.items()}
    if isinstance(v,(tuple,list)):return [strings(q) for q in v]
    return v

class Engine:
    def __init__(self, data, floor, budget=4096):
        self.original=np.array(data); self.floor=mp.mpf(floor);self.budget=budget
        xx=list(map(mp.mpf,data));self.n=len(xx);n=self.n
        self.center=mp.fsum(xx)/n
        self.unit=mp.sqrt(mp.fsum((q-self.center)**2 for q in xx)/n)
        self.x=[(q-self.center)/self.unit for q in xx]
        self.y=np.array(list(map(float,self.x))); self.cache={};self.heap=[]
        self.margin=mp.mpf('1e-48')
        self.normal=-n*(mp.log(2*mp.pi)/2+mp.mpf('.5'))
        self.A=(mp.fsum(q**4 for q in self.x)-3*n)/4
        self.B=mp.fsum(q**4/4-q**6/6 for q in self.x)+mp.fsum(q**3 for q in self.x)**2/(2*n)+(n-mp.fsum(q**4 for q in self.x))**2/(4*n)
        counts={}
        for q in data:counts[q]=counts.get(q,0)+1
        self.k=max(counts.values());self.atoms=[mp.mpf(q) for q,k in counts.items() if k*(1+floor)==n*floor]
        self.sorted=sorted(self.x);sy=np.sort(self.y)
        self.gaps=np.array([min(sy[k:]-sy[:-k])/2 for k in range(1,n)])
        self.exact_gaps={}
        self.L,self.U=self.containment(self.floor)

    def containment(self,v):
        n=self.n;ks=np.arange(1,n)
        proposals=self.gaps**2*((n-ks)/n-ks/(n*float(v)))
        for j in np.argsort(proposals)[::-1]:
            k=int(j)+1
            if k not in self.exact_gaps:
                self.exact_gaps[k]=min(self.sorted[i+k]-self.sorted[i] for i in range(n-k))/2
            square=self.exact_gaps[k]**2*((n-k)/mp.mpf(n)-k/(n*v))
            if square>0:return mp.sqrt(square),(self.sorted[-1]-self.sorted[0])*mp.sqrt(1+1/v)
        return mp.mpf(0),(self.sorted[-1]-self.sorted[0])*mp.sqrt(1+1/v)

    def value(self,v,m,t):
        z=[(q-m)*mp.exp(-t) for q in self.x]
        # Relative normalizer and total likelihood, evaluated with enough
        # precision for all v <= 1e30. No binary64 full-LL subtraction.
        c=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v*mp.pi)/2
        return self.n*(c-t)-(v+1)/2*mp.fsum(mp.log1p(q*q/v) for q in z)-self.normal

    def scores(self,v,m,t):
        s=mp.exp(t);z=[(q-m)/s for q in self.x]
        return (-mp.fsum((v+1)*q/(s*(v+q*q)) for q in z),
                self.n-mp.fsum((v+1)*q*q/(v+q*q) for q in z))

    def at(self,v):
        v=mp.mpf(v)
        if v in self.cache:return self.cache[v]
        if len(self.cache)>=self.budget:raise BudgetError('profile budget')
        if v>mp.mpf('1e30'):raise ArithmeticError('precision domain')
        rounding=self.margin+4096*mp.eps*(self.n+1)*(1+v*mp.log1p(v)+mp.fsum(abs(q) for q in self.x))
        if v==self.floor and self.atoms:
            vals=[]
            for b in self.atoms:
                b=(b-self.center)/self.unit
                c=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v*mp.pi)/2
                C=self.n*c+(v+1)*(self.n-self.k)*mp.log(v)/2-(v+1)*mp.fsum(mp.log(abs(q-b)) for q in self.x if q!=b)
                vals.append(C-self.normal)
            delta=max(vals)
            q={'v':v,'lower':delta-rounding,'upper':delta+rounding,'collapse':True}
        else:
            if not self.L>0:
                L,U=self.containment(v)
            else:L,U=self.L,self.U
            if not L>0:raise ArithmeticError('nuisance containment')
            def f(p):
                val,g,_,_=study.evaluate(self.y,float(v),p);return -val,g
            ans=minimize(f,[0,0],jac=True,method='BFGS',options={'gtol':1e-10,'maxiter':300})
            m,t=mp.findroot(lambda m,t:self.scores(v,m,t),tuple(map(mp.mpf,ans.x)),tol=mp.mpf('1e-55'),maxsteps=80)
            g=self.scores(v,m,t);a=mp.sqrt(v)*mp.exp(t)
            D=max(mp.acosh(1+((m-mm)**2+(a-mp.sqrt(v)*ss)**2)/(2*a*mp.sqrt(v)*ss))
                  for mm in [min(self.x),max(self.x)] for ss in [L,U])
            val=self.value(v,m,t);error=mp.sqrt((a*g[0])**2+g[1]**2)*D+rounding*(1+a)*(1+D)
            q={'v':v,'lower':val-rounding,'upper':val+error,'m':m,'t':t,'delta':val,'score_bound':error}
        self.cache[v]=q
        return q

    def tail(self, witness):
        # Strictly negative slope: use the local nuisance-ball proof, with
        # interval input moments, rather than subtracting two full values.
        if self.A < -mp.mpf('1e-35'):
            G=mp.sqrt(mp.fsum(abs(q*(1-q*q)) for q in self.x)**2+mp.fsum(abs(q*q*(1-q*q)) for q in self.x)**2)
            for r in map(mp.mpf,['.01','.03','.1']):
                for j in range(100):
                    h=mp.mpf(2)**(-j-1)
                    zl=[min((q-r)*mp.exp(-r),(q-r)*mp.exp(r)) for q in self.x]
                    zh=[max((q+r)*mp.exp(-r),(q+r)*mp.exp(r)) for q in self.x]
                    zmax=[max(l*l,u*u) for l,u in zip(zl,zh)]
                    zmin=[0 if l<=0<=u else min(l*l,u*u) for l,u in zip(zl,zh)]
                    if h*max(zmax)>=1:continue
                    den=[(1+h*q)**2 for q in zmax]
                    cross=2*(1+h)*mp.exp(r)*max(abs(mp.fsum(min(l,l/d) for l,d in zip(zl,den))),abs(mp.fsum(max(u,u/d) for u,d in zip(zh,den))))
                    lam=min(mp.exp(-2*r)*mp.fsum((1-h*q)/d for q,d in zip(zmax,den)),2*mp.fsum(q/d for q,d in zip(zmin,den)))-cross
                    if lam<=self.margin or lam*r-h*G<=self.margin:continue
                    K=mp.fsum(q**4 for q in self.x)/4+self.n*h/24+G*G/(2*lam)
                    if -self.A/2-h*K<=self.margin:continue
                    return {'kind':'signed','N':1/h,'upper':mp.mpf(0),'A':self.A,'K':K,'radius':r,'curvature':lam,
                            'inward_margin':lam*r-h*G,'slope_margin':-self.A/2-h*K}
        if witness>self.margin:
            L,U=self.containment(max(mp.mpf(2),self.floor+1))
            if L<=0:raise ArithmeticError('tail scale containment')
            C=mp.mpf(3)*self.n*((self.sorted[-1]-self.sorted[0])/L)**4/8
            N=max(mp.mpf(1000),2*C/witness)
            return {'kind':'unsigned','N':N,'upper':C/N,'C':C}
        return {'kind':'tie','A':self.A,'B':self.B}

    def boundary_tube(self):
        # Endpoint may be attained Cauchy or excluded positive-scale t2.
        q=self.at(self.floor)
        if q.get('collapse'):return None
        v0=float(self.floor);p=np.array([float(q['m']),float(q['t']+mp.log(self.floor)/2),1.])
        yi=roots.I.pad(self.y,self.y)
        for h in [.01,.003,.001,.0003,.0001,.00003,.00001,.000003,.000001]:
            for r in [h,3*h,10*h,30*h]:
                lo=p-np.array([r,r,0]);hi=p+np.array([r,r,h])
                hl,hh,gl,gh=roots.enclosure(yi,lo,hi,v0,True)
                mid=(hl[:2,:2]+hh[:2,:2])/2;rad=(hh[:2,:2]-hl[:2,:2])/2
                lam=np.linalg.eigvalsh(mid)[0]-np.linalg.norm(rad,ord='fro')-1e-10
                cl=p.copy();ch=p.copy();ch[2]+=h
                _,_,sl,sh=roots.enclosure(yi,cl,ch,v0,True)
                G=np.linalg.norm(np.maximum(abs(sl[:2]),abs(sh[:2])))
                if lam>0 and G<lam*r and gl[2]>1e-10:
                    return {'shape_interval':[v0,v0*(1+h)],'nuisance_radius':r,'inward_margin':float(lam*r-G),
                            'negative_profile_slope_upper':float(-gl[2]/v0)}
        return None

    def boundary_chord(self):
        # Keep the exact unknown endpoint P(q) on both sides before
        # bounding the secant. No constant endpoint width enters the seam.
        q=self.floor;endpoint=self.at(q)
        for rel in ['.1','.03','.01','.003','.001','.0003','.0001','.00003','.00001']:
            h=q*mp.mpf(rel);right=self.at(q+h)
            slope=(right['upper']-endpoint['lower'])/h+self.n*h/(2*q*q)
            if slope < -self.margin:
                return {'shape_interval':[float(q),float(np.nextafter(float(q+h),float(q)))],'method':'relative_endpoint_chord',
                        'slope_upper':slope,'secant_upper':(right['upper']-endpoint['lower'])/h}
        return None

    def boundary_box(self):
        q=self.at(self.floor)
        if q.get('collapse'):return None
        v0=float(self.floor);p=np.array([float(q['m']),float(q['t']+mp.log(self.floor)/2),1.])
        yi=roots.I.pad(self.y,self.y)
        _,H=roots.equations(self.y,p,v0);C=np.linalg.inv(H[:2,:2])
        _,_,gl,gh=roots.enclosure(yi,p,p,v0,True)
        el,eh=roots.transform(C,gl[:2],gh[:2]);e=np.maximum(abs(el),abs(eh))
        for width in [1e-10,1e-9,1e-8,1e-7,1e-6]:
            r=width*np.maximum(1,abs(p[:2]));lo=p.copy();hi=p.copy();lo[:2]-=r;hi[:2]+=r
            hl,hh=roots.enclosure(yi,lo,hi,v0)
            cl,ch=roots.transform(C,hl[:2,:2],hh[:2,:2])
            Q=np.maximum(abs(np.eye(2)-cl),abs(np.eye(2)-ch));ratio=(e+Q@r)/r
            if max(ratio)<1 and np.linalg.norm(Q,ord=np.inf)<1:
                return {'lo':lo[:2].tolist(),'hi':hi[:2].tolist(),'image_ratio':ratio.tolist(),
                        'nu':v0,'coordinate_center':str(self.center),'coordinate_unit':str(self.unit)}
        return None

    def solve(self, isolation=None):
        try:return self._solve(isolation)
        except BudgetError:return {'outcome':'budget','profiles':len(self.cache),'reason':'profile budget'}
        except (ArithmeticError,ValueError,ZeroDivisionError,np.linalg.LinAlgError) as error:
            return {'outcome':'numerical_failure','profiles':len(self.cache),'reason':str(error)}

    def _solve(self, isolation=None):
        if self.budget==0:return {'outcome':'budget','profiles':0}
        if self.k*(1+float(self.floor))>self.n*float(self.floor):return {'outcome':'collapse_unbounded','profiles':0}
        if len(set(self.original))==2 and self.k*2==self.n:
            return {'outcome':'normal','method':'balanced_two_point_theorem','profiles':0}
        if self.floor==2 and self.k*3==self.n*2 and len(set(self.original))==3:
            support=sorted(set(map(Fraction,self.original)))
            if support[0]+support[2]==2*support[1] and list(self.original).count(float(support[0]))*6==self.n and list(self.original).count(float(support[2]))*6==self.n:
                return {'outcome':'collapse','method':'symmetric_critical_three_point_theorem','profiles':0,
                        'atom':str(support[1]),'half_range':str(support[2]-support[1]),'seam_A_exact':'0','seam_B_exact':str(Fraction(self.n,4))}
        candidates=[]
        for v in np.geomspace(float(self.floor),1000,33):candidates.append(self.at(float(v)))
        best=max(candidates,key=lambda q:q['lower'])
        # A feasible lower bound suffices for every global exclusion.
        witness=max(mp.mpf(0),best['lower'])
        tail=self.tail(witness)
        if tail['kind']=='tie':return {'outcome':'seam_tie','A':self.A,'B':self.B,'profiles':len(self.cache)}
        N=tail['N'];self.at(N)
        target='normal';window=None;endpoint_box=None
        if best['lower']>0:
            if best['v']==self.floor:
                window=self.boundary_chord()
                if best.get('collapse'):target='collapse'
                else:
                    target='cauchy' if self.floor==1 else 't2'
                    endpoint_box=self.boundary_box()
                    if endpoint_box is None:window=None
            elif isolation and isolation['outcome']=='root_and_tube':
                window=isolation['tube'];target='finite'
            if window is None:
                # Nomination only. root_box and tube, not this proposal,
                # determine whether any finite output can be certified.
                nu=float(best['v']);m=float(self.center+self.unit*best['m']);s=float(self.unit*mp.exp(best['t']))
                isolation=roots.isolate(self.original,nu,m,s)
                if isolation['outcome']=='root_and_tube' and isolation['coefficients']['nu'][0]>float(self.floor):window=isolation['tube'];target='finite'
            if window is None:return {'outcome':'root_or_endpoint_neighbourhood_failure','profiles':len(self.cache),'tail':tail,'isolation':isolation}
            l,h=map(mp.mpf,window['shape_interval']);self.at(l);self.at(h)
            if l<self.floor:return {'outcome':'neighbourhood_crosses_domain','profiles':len(self.cache)}
            if target=='finite':
                v0=isolation['v0'];p=isolation['point'];v=mp.mpf(v0)*mp.mpf(p[2])
                m=mp.mpf(isolation['center'])+mp.mpf(isolation['unit'])*mp.mpf(p[0]);s=mp.mpf(isolation['unit'])*mp.exp(mp.mpf(p[1]))/mp.sqrt(v)
                witness=self.value(v,(m-self.center)/self.unit,mp.log(s/self.unit))-self.margin
            else:witness=self.at(self.floor)['lower']
        serial=0;pruned=0
        def push(l,h):
            nonlocal serial,pruned
            if window:
                wl,wh=map(mp.mpf,window['shape_interval'])
                if l>=wl and h<=wh:return
            upper=max(self.at(l)['upper'],self.at(h)['upper'])+self.n*(h-l)**2/(8*l*l)
            if upper<witness:pruned+=1;return
            heapq.heappush(self.heap,(-upper,serial,l,h));serial+=1
        knots=sorted(k for k in self.cache if self.floor<=k<=N)
        for l,h in zip(knots[:-1],knots[1:]):push(l,h)
        try:
            while self.heap:
                _,_,l,h=heapq.heappop(self.heap);mid=mp.sqrt(l*h);self.at(mid)
                push(l,mid);push(mid,h)
        except BudgetError:
            return {'outcome':'budget','profiles':len(self.cache),'target':target,'tail':tail,'pruned':pruned,'window':window,
                    'unresolved_interval':[l,h],'witness':witness,'isolation':isolation}
        if tail['upper']>witness:return {'outcome':'tail_comparison_failure','tail':tail,'witness':witness}
        return {'outcome':target,'profiles':len(self.cache),'pruned':pruned,'tail':tail,'witness':witness,'window':window,'isolation':isolation,'endpoint_box':endpoint_box}

def main():
    cases=json.loads((OLD/'inputs.json').read_text())
    isolations=json.loads((HERE/'roots.json').read_text())
    rows=[]
    for case in cases:
        for family,floor in [('lst',1),('std',2)]:
            if len(set(case['x']))==1:continue
            iso=next((q for q in isolations if q['name']==case['name'] and q['family']==family),None)
            start=time.perf_counter()
            try:q=Engine(case['x'],floor).solve(iso)
            except (ArithmeticError,ValueError,ZeroDivisionError,RuntimeError) as e:q={'outcome':'numerical_failure','reason':str(e)}
            rows.append({'name':case['name'],'family':family,'seconds':time.perf_counter()-start,**strings(q)})
            print(case['name'],family,q['outcome'],q.get('profiles'),flush=True)
            (HERE/'global.json').write_text(json.dumps(rows,indent=2,allow_nan=False)+'\n')

if __name__=='__main__':main()
