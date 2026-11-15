"""Emulate burr_cert_detail::scale_fit's root search (scale_certificate.hpp 36-110) in double for the scale-held cases."""
import sys, json, math
sys.path.insert(0,'dev/validation'); sys.path.insert(0,'dev/research/evaluations/2026-11-15-refine-causes-data')
from strict_common import archive_data
import mpmath as mp
import emu_burr_fixed_scale as ef
mp.mp.dps=60
def softplus(z): return max(z,0.)+math.log1p(math.exp(-abs(z)))
def sigmoid(z): return 1/(1+math.exp(-z)) if z>=0 else math.exp(z)/(1+math.exp(z))
def point(z,t):
    g=gp=rem=d=0.0
    for v in z:
        w=t*v; g+=softplus(w); gp+=v*sigmoid(w); rem+=softplus(-w); d+=v*sigmoid(-w)
    n=len(z); return n/t-n*gp/g+d, g
def run(x,scale):
    z=[]; unit=0.0
    for v in x:
        l=math.log1p((v-scale)/scale) if (v>=scale/2 and v/2<=scale) else math.log(v)-math.log(scale)
        z.append(l); unit=max(unit,abs(l))
    z=[l/unit for l in z]
    lo,hi=0.0,1.0
    while True:
        s,g=point(z,hi)
        if s<0: break
        lo=hi; hi*=2
    t=(lo+hi)/2; probes=0
    while True:
        s,g=point(z,t); probes+=1
        if s>0: lo=t
        else: hi=t
        if hi-lo<=1e-13*t: break
        t=(lo+hi)/2
    t=(lo+hi)/2; return t,unit,z,probes
if __name__=='__main__':
    a_=archive_data(); ex=json.load(open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json')); out=[]
    for case in (688,689,9424,9884):
        e=[e for e in ex if e['family']=='burr' and e['mode']=='double' and e['case']=='%06d'%case][0]
        sc=e['held']['scale']; x=list(a_[case]); t,unit,z,pr=run(x,sc)
        ks,cs=ef.star(x,mp.mpf(sc)); c=t/unit; n=len(z); g=sum(softplus(t*v) for v in z); k=n/g
        ret=(float(e['est']['shape1']),float(e['est']['shape2']))
        # score noise: perturb score by 1e-16 scale and see root movement
        o=dict(case=case,bisect_c=c,bisect_k=k,bisect_rel_err=(float((c-cs)/cs),float((k-ks)/ks)),returned_rel_err=(float((ret[0]-ks)/ks),float((ret[1]-cs)/cs)),
               equal_to_returned=(c==ret[1] and k==ret[0]),probes=pr,unit=unit)
        # score's slope in t (relative): d score / d log t at t, and rounding noise of the score terms
        s0,_=point(z,t); sp,_=point(z,t*(1+1e-6)); o['dscore_dlogt']=(sp-s0)/1e-6
        o['score_term_scale']=sum(abs(v) for v in z)
        print(json.dumps(o)); out.append(o)
    json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/emu_burr_scalefit.json','w'),indent=1)
