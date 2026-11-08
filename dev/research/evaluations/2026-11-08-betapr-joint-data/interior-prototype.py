"""Experimental shape-rectangle certificate, not used by the fitter.
This records the practical remaining interior problem, including work and
coefficient enclosures. No assertion of a global certificate is made here.
"""
import heapq,json,math,pathlib,time,sys
import numpy as np
from scipy.special import betaln,digamma
from scipy.optimize import brentq
root=pathlib.Path(__file__).resolve().parents[4]
data=pathlib.Path(__file__).resolve().parent
rows=[list(map(float,s.split())) for s in (data/'probe.txt').read_text().splitlines()[:2]]
golden=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/betapr/golden.json').read_text())['data']
results=[]
wide="--wide" in sys.argv
width_tolerance=1e-7 if wide else 1e-10
for x,row in zip([[.03,.1,.4,.7,1,2,4,12],golden],rows):
    start=time.monotonic();logs=np.log(np.array(x));n=len(x);lm=float(logs.mean())
    L,lo,H,G,IG,Q,P=row[3:10];L/=n
    cache={};cells=0;discard_grad=0;discard_value=0;retained=[];seq=0
    def point(p,q):
        key=(p,q)
        if key in cache:return cache[key]
        def score(t):
            u=1/(1+np.exp(t-logs))
            return (p+q)*float(u.mean())-p
        left=math.log(q/p)+float(logs.min());right=math.log(q/p)+float(logs.max())
        t=brentq(score,left,right,xtol=1e-13)
        A=float(np.logaddexp(0,t-logs).mean());B=float(np.logaddexp(0,logs-t).mean())
        lb=float(betaln(p,q));ll=-lb-p*A-q*B-lm
        value=(ll,t,A,B,lb)
        cache[key]=value
        return value
    queue=[]
    def add(pl,pu,ql,qu):
        global seq,cells,discard_grad,discard_value
        cells+=1
        # h is monotone in both coordinates.
        if pl*ql/(pl+ql)>=H or pu*qu/(pu+qu)<=lo:return
        corners=[(p,q,point(p,q)) for p in (pl,pu) for q in (ql,qu)]
        p=math.sqrt(pl*pu);q=math.sqrt(ql*qu);mid=point(p,q)
        psi=digamma(p+q);da=psi-digamma(p);db=psi-digamma(q)
        upper=max(pt[0]+pt[4]-mid[4]+da*(a-p)+db*(b-q) for a,b,pt in corners)
        error=2e-12*(1+pu+qu+abs(upper))
        upper+=error
        if upper<L:discard_value+=1;return
        # Scale decreases in p and increases in q.
        low_t=corners[2][2][1];high_t=corners[1][2][1]
        Ahi=float(np.logaddexp(0,high_t-logs).mean());Alo=float(np.logaddexp(0,low_t-logs).mean())
        Bhi=float(np.logaddexp(0,logs-low_t).mean());Blo=float(np.logaddexp(0,logs-high_t).mean())
        da_lo=digamma(pu+ql)-digamma(pu)-Ahi
        da_hi=digamma(pl+qu)-digamma(pl)-Alo
        db_lo=digamma(pl+qu)-digamma(qu)-Bhi
        db_hi=digamma(pu+ql)-digamma(ql)-Blo
        grad_error=error if wide else 2e-13
        if da_lo>grad_error or da_hi<-grad_error or db_lo>grad_error or db_hi<-grad_error:
            discard_grad+=1;return
        if max(math.log(pu/pl),math.log(qu/ql))<width_tolerance:
            retained.append((pl,pu,ql,qu,low_t,high_t,upper));return
        seq+=1;heapq.heappush(queue,(-upper,seq,(pl,pu,ql,qu)))
    add(lo,P,lo,Q)
    while queue and cells+len(cache)<128000:
        _,_,(pl,pu,ql,qu)=heapq.heappop(queue)
        if math.log(pu/pl)>math.log(qu/ql):
            mid=math.sqrt(pl*pu);add(pl,mid,ql,qu);add(mid,pu,ql,qu)
        else:
            mid=math.sqrt(ql*qu);add(pl,pu,ql,mid);add(pl,pu,mid,qu)
    out={'n':n,'cells':cells,'profile_evaluations':len(cache),'queue_left':len(queue),
         'retained_cells':len(retained),'discard_gradient':discard_grad,'discard_value':discard_value,
         'seconds':time.monotonic()-start}
    if retained:
        out['retained_cells_enclosure']=[min(s[0] for s in retained),max(s[1] for s in retained),min(s[2] for s in retained),max(s[3] for s in retained),math.exp(min(s[4] for s in retained)),math.exp(max(s[5] for s in retained))]
    results.append(out)
    (data/('interior-prototype-wide.json' if wide else 'interior-prototype.json')).write_text(json.dumps(results,indent=2)+'\n')
    print(json.dumps(out),flush=True)
