"""Independent mpmath checks of Student boxes, centers and boundary inequalities."""
import json
from fractions import Fraction
from math import log
from pathlib import Path
import mpmath as mp
import numpy as np
from boxes import I, ROOT, load, workloads, held_std_cutoff, held_mean_floor
from exact_modes import RADII, COUNTS, score, real

mp.mp.dps=80


def density(x,m,s,h):
    z=(x-m)/s
    if h==0:
        return -mp.log(2*mp.pi)/2-mp.log(s)-z*z/2
    v=1/h
    return (mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v*mp.pi)/2-
            mp.log(s)-(v+1)/2*mp.log1p(h*z*z))


def compact_scale(x,m,s,h,values,weights=None):
    if h==0:
        return
    if weights is not None:
        return
    xs=sorted(x)
    n=len(xs)
    v=1/h
    lo=max(min((xs[i+j]-xs[i])**2/4 for i in range(n-j))*((n-j)-j/v)/n
           for j in [1,n//4,n//3,n//2,n-1])
    hi=mp.fsum((x_i-m)**2 for x_i in xs)/n
    assert lo<=s*s<=hi+mp.mpf('1e-65'), (lo,s*s,hi,values)


def derivs(model, widths, params):
    # Check normalized, exactly represented box inputs. Golden root checks
    # below independently use raw input and exact normalization.
    m,e,h = params
    Rad=np.array(widths)
    box=[I(x-y,x+y) for x,y in zip(params,Rad)]
    gi,Hi=model.interval(*box)
    ix=model.free+[2] if model.held_scale is not None else [0,1,2]
    x=[mp.mpf(float(a)) for a in model.x]
    weights=[mp.mpf(float(a)) for a in model.w]

    def fun(m,e,h):
        if model.held_scale is not None:
            e=mp.mpf(model.held_scale)
            if model.std:
                e+=mp.log1p(-2*h)/2
        s=mp.exp(e)
        v=1/h
        C=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v*mp.pi)/2-e
        return mp.fsum(w*(C-(v+1)/2*mp.log1p(h*((t-m)/s)**2))
                       for t,w in zip(x,weights))
    values=0
    for direction in (-.83, .31):
        p=tuple(mp.mpf(float(a+direction*b)) for a,b in zip(params,Rad))
        value=fun(*p)
        assert value<=model.likelihood_upper(*box), (value,model.likelihood_upper(*box))
        values+=1
        for i in ix:
            u=[0]*3
            u[i]=1
            g=mp.diff(fun,p,tuple(u))
            assert gi[i].lo<=g<=gi[i].hi, (i, gi[i].lo,g,gi[i].hi,p)
            values+=1
            for j in ix:
                if j<i:
                    continue
                t=u.copy()
                t[j]+=1
                H=mp.diff(fun,p,tuple(t))
                assert Hi[i][j].lo<=H<=Hi[i][j].hi, (i,j,Hi[i][j].lo,H,Hi[i][j].hi,p)
                values+=1
    return values


def raw_root(name, record):
    fam,mask=name.split('_',1)
    if fam not in ('lst','std','ht'):
        return None
    source=load(fam)
    hold=source['estimates']
    x=[mp.mpf(t) for t in source['data']]
    n=len(x)
    center=mp.fsum(x)/n
    unit=mp.sqrt(mp.fsum((t-center)**2 for t in x)/n)
    p=tuple(map(mp.mpf,record['parameters']))
    held_m = mp.mpf(0) if fam=='ht' else (
        mp.mpf(hold['mu' if fam=='lst' else 'mean']) if mask=='held_mean' else None)
    held_s = mp.mpf(hold['sigma' if fam=='lst' else 'sd']) if mask=='held_scale' else None
    active=[i for i,v in enumerate((held_m,held_s,None)) if v is None]

    def unpack(args):
        z=list(p)
        for i,val in zip(active,args):
            z[i]=val
        m=center+unit*z[0] if held_m is None else held_m
        h=z[2]
        if held_s is None:
            s=unit*mp.exp(z[1])
        else:
            s=held_s if fam=='lst' else held_s*mp.sqrt(1-2*h)
        return m,s,h

    def value(*args):
        m,s,h=unpack(args)
        return mp.fsum(density(t,m,s,h) for t in x)/n
    markers=[tuple(int(j==i) for j in range(len(active))) for i in range(len(active))]
    sol=mp.findroot(lambda *a:[mp.diff(value,a,d) for d in markers],
                    tuple(p[i] for i in active),tol=mp.mpf('1e-66'))
    vv=value(*sol)+(mp.log(2) if fam=='ht' else 0)
    assert abs(vv-record['avg_loglik'])<mp.mpf('2e-12')
    for q,i,width in zip(sol,active,record['root_full_widths']):
        assert abs(q-p[i])<width/2, (name,p[i],q,width)
    m,s,h=unpack(sol)
    if held_s is None:
        compact_scale(x,m,s,h,record)
    sd=s/mp.sqrt(1-2*h) if fam=='std' else s
    return {'location':mp.nstr(m,25), 'public_scale':mp.nstr(sd,25),
            'shape':mp.nstr(1/h,25), 'total_loglik':mp.nstr(vv*n,25)}


def mode_checks():
    for v,sign in [(4,1),(6,-1),(20,-1),(50,1),(100,-1)]:
        ab=score(v)
        assert all(sign*a>0 for a in ab)
    radii=list(map(real,RADII))
    def val(e,v):
        return mp.fsum(w*density(r,mp.mpf(0),mp.exp(e),1/v)
                       for r,w in zip(radii,COUNTS))/sum(COUNTS)
    roots=[]
    q=[r*r for r in radii]
    n=sum(COUNTS)

    def prof(v):
        lo,hi=mp.mpf(0),max(q)
        for _ in range(155):
            m=(lo+hi)/2
            sc=(v+1)*mp.fsum(w*a/(v*m+a) for w,a in zip(COUNTS,q))-n
            if sc>0:
                lo=m
            else:
                hi=m
        e=mp.log((lo+hi)/2)/2
        return e,mp.diff(lambda shape:val(e,shape),v)

    for left,right in [(4,6),(20,50),(50,100)]:
        a,b=mp.mpf(left),mp.mpf(right)
        sign=mp.sign(prof(a)[1])
        assert sign*prof(b)[1]<0
        for _ in range(95):
            c=(a+b)/2
            if sign*prof(c)[1]>0:
                a=c
            else:
                b=c
        v=(a+b)/2
        e,gv=prof(v)
        assert abs(gv)<mp.mpf('1e-27')
        roots.append({'shape':mp.nstr(v,25),'ordinary_scale':mp.nstr(mp.exp(e),25),
                      'avg_loglik':mp.nstr(val(e,v),25)})
    return roots


def boundary_checks():
    results={}
    # Critical all-free ordinary scale collar, both attained-shape floors.
    for floor,k,other in [(1,3,[1,2,5]), (2,4,[1,3])]:
        q=mp.mpf(floor); W=mp.mpf('0.25'); n=k+len(other)
        logs=max(abs(mp.log((min(other)-W)**2)),
                 abs(mp.log((max(other)+W)**2+1)))
        K=n/q+len(other)*logs/2
        a0=min(mp.mpf('0.5'),mp.exp(-1-K/len(other)))
        def ell(v,a,u):
            C=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(mp.pi)/2
            return (n*C+(v*len(other)-k)*mp.log(a)-(v+1)/2*
                    (k*mp.log1p((u/a)**2)+mp.fsum(mp.log((d-u)**2+a*a) for d in other)))
        C=ell(q,mp.mpf('1e-45'),0)
        for j in (1,2,4,10,32):
            a=a0**j
            for u in (-W,-a*a,a*a,W):
                for b in (mp.mpf('0.00000001'),mp.mpf('.01'),mp.mpf('1')):
                    v=q+b
                    assert ell(v,a,u)-ell(q,a,u) <= b*(K+len(other)*mp.log(a))
                    assert ell(v,a,u)<C+mp.mpf('1e-70')
        results[f'critical_ordinary_{floor}']={'checks':60,'a0':float(a0)}
    # Held-sd critical collar. The tail must approach the contact height
    # from below, even for moving centers near the largest atom.
    x=list(map(mp.mpf,[0,0,0,0,1,5]))
    k,sd=4,mp.mpf(1)
    S=mp.log(5)
    L=2*(1/mp.mpf('.75')+1/mp.mpf('4.75'))*2
    D=len(x)/2+abs(S)+L**2/(3*k)
    W=min(mp.mpf('.01'),mp.exp(-1-2*D/(len(x)-k)))
    C=-len(x)*mp.log(2)-3*S
    for j in (1,2,3,4,8):
        w=W**j
        for m in (0,mp.sqrt(w),-mp.sqrt(w),w,mp.mpf('.25'),mp.mpf('-.25')):
            ll=mp.fsum(density(r,m,sd*mp.sqrt(w/(w+2)),1/(w+2)) for r in x)
            assert ll<=C+w*(D+(len(x)-k)*mp.log(w)/2)
    results['critical_held_sd']={'checks':30,'w0':float(W)}
    return results


if __name__=='__main__':
    script=Path(__file__)
    data=json.loads((script.parent/'boxes.json').read_text())
    results={'golden_roots':{},'box_probes':{},'boundary':boundary_checks(),
             'mode_roots':mode_checks()}
    for name,model,hmax,_,collar in workloads():
        row=data[name]
        if 'error' in row or row.get('pending') != 0:
            continue
        if name in ('ht_two_modes',) or name.startswith(('two_interior','normal_local')):
            hmax=row['hmax']
        if hmax is None:
            hmax=row['hmax']
        p=np.array(row['parameters'])
        if model.held_scale is not None:
            p[1]=model.eta(p[2])
        count=0
        # Probe a near-normal box, a central box and a near-floor box,
        # at two geometric widths and two interior points per box.
        for frac in (.04,.46,.93):
            point=p.copy()
            point[2]=hmax*frac
            for relative in (1e-5,.014):
                count+=derivs(model,[relative,relative,relative*hmax],point)
        results['box_probes'][name]=count
        if name.startswith(('lst_','std_')) or name=='ht_golden':
            results['golden_roots'][name]=raw_root(name,row)
        print(f'{name}: {count} independent box comparisons',flush=True)
    print(json.dumps(results,indent=2))
