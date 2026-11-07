"""60-digit independent checks of the Gompertz profile envelope inequalities."""
import json
import sys
import mpmath as m
m.mp.dps=60
samples=[]
for line in open(sys.argv[1]):
    row=line.split()
    if row[0]=='sample':samples.append(list(map(m.mpf,row[2:])))

def profile(x,b):
    ev=[m.exp(-b*v) for v in x]; av=[1-v for v in ev]; n=len(x)
    def score(e):return sum(a/(1+e*a)-v for a,v in zip(av,ev))
    if score(0)<=0:e=m.mpf(0)
    else:
        lo=m.mpf(0);hi=n/sum(ev)
        for _ in range(180):
            mid=(lo+hi)/2
            if score(mid)>0:lo=mid
            else:hi=mid
        e=(lo+hi)/2
    ll=sum(m.log(b)-b*v-e*t+m.log1p(e*a) for v,t,a in zip(x,ev,av))
    sb=n/b-sum(x)+sum(e*v*t*(1+1/(1+e*a)) for v,t,a in zip(x,ev,av))
    bb=-n/b**2-sum(e*v*v*t+e*(1+e)*v*v*t/(1+e*a)**2 for v,t,a in zip(x,ev,av))
    cross=sum(v*t*(1+1/(1+e*a)**2) for v,t,a in zip(x,ev,av))
    den=sum((a/(1+e*a))**2 for a in av)
    curvature=bb+cross**2/den if e else bb
    return e,ll,sb,curvature

checks=0; worst=m.mpf('-inf')
for x in samples[::3]:
    x=[v/max(x) for v in x];n=len(x);rate=n/sum(x)
    for multiplier in (m.mpf('.7'),m.mpf('3')):
        lo=rate*multiplier/2;hi=rate*multiplier*2;mid=m.sqrt(lo*hi)
        el=profile(x,lo)[0];eh=profile(x,hi)[0]
        al=[1-m.exp(-lo*v) for v in x]
        cross=sum(v*m.exp(-lo*v)*(1+1/(1+el*a)**2) for v,a in zip(x,al))
        den=sum((a/(1+eh*a))**2 for a in al)
        negative=sum(el*v*v*m.exp(-hi*v)+el*(1+el)*v*v*m.exp(-hi*v)/(1+eh*(1-m.exp(-hi*v)))**2 for v in x)
        M=-n/hi**2-negative+cross**2/den
        _,ll,score,_=profile(x,mid)
        for fraction in (m.mpf(0),m.mpf('.25'),m.mpf('.75'),m.mpf(1)):
            b=lo+(hi-lo)*fraction;e,v,s,curv=profile(x,b)
            assert el-m.mpf('1e-45')<=e<=eh+m.mpf('1e-45')
            assert curv<=M+m.mpf('1e-40')
            bound=ll+score*(b-mid)+max(M,0)*(b-mid)**2/2
            gap=v-bound;worst=max(worst,gap)
            assert gap<m.mpf('1e-40'),(b,gap)
            checks+=1
print(json.dumps({'precision_digits':60,'boxes':16,'interior_checks':checks,'maximum_loglik_minus_upper':str(worst)},indent=2))
