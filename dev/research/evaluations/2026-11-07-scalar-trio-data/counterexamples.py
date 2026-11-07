"""Independent analytic held-alpha fatigue counterexamples, at 60 digits."""
import json
import mpmath as m
m.mp.dps=60
records=[]
for x,lo,hi in [([m.mpf(1)],(7-3*m.sqrt(5))/2,(7+3*m.sqrt(5))/2),([m.mpf('.5'),m.mpf(2)],(47-3*m.sqrt(201))/20,(47+3*m.sqrt(201))/20)]:
    def ll(b):return sum(m.log(v+b)-m.log(6)-m.log(b)/2-3*m.log(v)/2-m.log(2*m.pi)/2-(v-b)**2/(18*v*b) for v in x)
    assert abs(m.diff(ll,lo))<m.mpf('1e-55')
    assert abs(m.diff(ll,hi))<m.mpf('1e-55')
    assert abs(ll(lo)-ll(hi))<m.mpf('1e-55')
    assert m.diff(ll,1,2)>0
    assert m.diff(ll,lo,2)<0 and m.diff(ll,hi,2)<0
    records.append({'data':list(map(str,x)),'alpha':3,'beta_maxima':[str(lo),str(hi)],'loglik_gain_over_beta_one':str(ll(lo)-ll(1))})
print(json.dumps({'precision_digits':60,'counterexamples':records},indent=2))
