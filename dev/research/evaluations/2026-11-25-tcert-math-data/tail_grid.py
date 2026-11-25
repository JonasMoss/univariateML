"""80-digit probes of scale, location, normal and lower-shape exterior bounds."""
import json
from pathlib import Path
import mpmath as mp
from boxes import workloads
from check import density

mp.mp.dps=80


def compact_probes(model,row):
    x=list(map(lambda t:mp.mpf(float(t)),model.x))
    weights=list(map(lambda t:mp.mpf(float(t)),model.w))
    weights=[w/mp.fsum(weights) for w in weights]
    N=mp.mpf(8)/row['hmax']
    q=1/mp.mpf(row['hmax'])
    p=1-max(weights)
    delta=min(b-a for a,b in zip(sorted(x),sorted(x)[1:]))/2
    incumbent=mp.mpf(row['avg_loglik'])+mp.log(model.unit)
    assert ((1+q)*p-1)>0 and delta>0
    A=-mp.log(2*mp.pi)/2
    upper=A-(q+1)*p*mp.log(delta**2/N)/2
    s0=mp.exp((incumbent-1-upper)/((q+1)*p-1))
    s1=mp.exp(A-incumbent+1)
    D=mp.exp(-incumbent)
    count=0

    def val(m,s,v):
        d=s if not model.std or model.held_scale is None else s*mp.sqrt(1-2/v)
        return mp.fsum(w*density(z,m,d,1/v) for w,z in zip(weights,x))

    for v in [q,(N+q)/2,N]:
        for m in [min(x),mp.mpf(0),max(x)]:
            for s in [s0,s0/10,s1,s1*10]:
                assert val(m,s,v)<incumbent
                count+=1
        for m in [min(x)-D,max(x)+D]:
            for s in [s1/10,s1,D]:
                assert val(m,s,v)<=-mp.log(2*D)<incumbent
                count+=1
    return count


def normal_probes(model,row):
    x=list(map(lambda t:mp.mpf(float(t)),model.x))
    weights=list(map(lambda t:mp.mpf(float(t)),model.w))
    weights=[w/mp.fsum(weights) for w in weights]
    m,e,_=list(map(mp.mpf,row['parameters']))
    if model.held_scale is not None:
        e=mp.mpf(model.held_scale)
    count=0
    for shift in (mp.mpf(0),mp.mpf('0.1')):
        s=mp.exp(e-shift)
        z2=[((z-m-shift)/s)**2 for z in x]
        A=mp.fsum(w*(p**2-2*p-1)/4 for w,p in zip(weights,z2))
        B=mp.fsum(w*p**2/4 for w,p in zip(weights,z2))
        for h in map(mp.mpf,['1e-12','1e-8','1e-4','0.01','0.1']):
            ll=mp.fsum(w*(density(z,m+shift,s,h)-density(z,m+shift,s,mp.mpf(0)))
                       for w,z in zip(weights,x))
            assert ll<=h*A+h*h*B+h**3/24
            count+=1
    return count


def lower_probes(model,row,half_t):
    x=[mp.mpf(float(z)) for z in model.x]
    weights=[mp.mpf(float(z)) for z in model.w]
    weights=[w/mp.fsum(weights) for w in weights]
    m=mp.mpf(model.held_m)
    inc=mp.mpf(row["avg_loglik"])+mp.log(model.unit)-(mp.log(2) if half_t else 0)
    count=0
    for f in map(mp.mpf,['1','0.1','1e-6']):
        v=f/mp.mpf(row['hmax'])
        U=mp.log(v/2)-mp.fsum(w*mp.log(abs(z-m)) for w,z in zip(weights,x))
        for s in map(mp.mpf,['1e-9','0.0001','0.3','1','10']):
            ll=mp.fsum(w*density(z,m,s,1/v) for w,z in zip(weights,x))
            assert ll<U<inc
            count+=1
    return count


def held_sd_probes(model,row):
    x=[mp.mpf(float(z)) for z in model.x]
    weights=[mp.mpf(float(z)) for z in model.w]
    weights=[w/mp.fsum(weights) for w in weights]
    d=mp.exp(mp.mpf(model.held_scale))
    inc=mp.mpf(row['avg_loglik'])+mp.log(model.unit)
    W=mp.mpf(row['collar']['excluded_w'])
    A=mp.loggamma((W+3)/2)-mp.loggamma((W+2)/2)-mp.log(mp.pi)/2-mp.log(d)
    stack=[(min(x),max(x))]
    cells=0
    samples=0
    while stack:
        lo,hi=stack.pop()
        delta=[max(mp.mpf(0),lo-z,z-hi) for z in x]
        good=[(w,v) for w,v in zip(weights,delta) if v>d*mp.sqrt(W)]
        slope=(3*mp.fsum(w for w,v in good)-1)/2
        U=A+slope*mp.log(W)-3*mp.fsum(w*mp.log(v/d) for w,v in good)
        if slope<=0 or U>=inc:
            assert hi-lo>d*mp.sqrt(W)/8
            mid=(lo+hi)/2
            stack.extend([(lo,mid),(mid,hi)])
            continue
        cells+=1
        for m in (lo,(lo+hi)/2,hi):
            for w0 in (W,W**2,W**5):
                h=1/(2+w0)
                s=d*mp.sqrt(w0/(2+w0))
                ll=mp.fsum(w*density(z,m,s,h) for w,z in zip(weights,x))
                assert ll<=U<inc
                samples+=1
    return {'accepted_cells':cells,'probes':samples}


if __name__=='__main__':
    result={}
    archive=json.loads((Path(__file__).parent/'boxes.json').read_text())
    for name,model,_,_,_ in workloads():
        row=archive[name]
        r={'normal':normal_probes(model,row)}
        if name in ('std_all_free','lst_all_free','two_interior_modes','two_interior_modes_lst',
                    'normal_local_and_finite_or_lower','normal_local_and_finite_or_lower_lst'):
            r['location_and_scale']=compact_probes(model,row)
        if name in ('lst_held_mean','ht_golden','ht_two_modes'):
            r['lower_shape']=lower_probes(model,row,name.startswith("ht_"))
        if name=='std_held_scale':
            r['lower_shape']=held_sd_probes(model,row)
        result[name]=r
    print(json.dumps(result,indent=2))
