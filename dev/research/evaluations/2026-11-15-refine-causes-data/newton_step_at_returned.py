"""Newton step of the 50-digit log likelihood at each returned point: |H^-1 g| in relative coordinates.
For algorithms whose stop is a Newton-step bound (bhatt certificate, burr, pareto3) this is the quantity the stop controls."""
import sys, json
sys.path.insert(0,'dev/validation')
from strict_common import archive_data
import mpmath as mp
mp.mp.dps=60
def bhatt_ll(x,mu,s,a):
    return sum(mp.log((mp.erf((v-mu+a)/(mp.sqrt(2)*s))-mp.erf((v-mu-a)/(mp.sqrt(2)*s)))/(4*a)) for v in x)
if __name__=='__main__':
    a_=archive_data(); ex=json.load(open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json'))
    e=[e for e in ex if e['family']=='bhatt' and e['mode']=='double'][0]
    x=[mp.mpf(v) for v in a_[363]]; s=mp.mpf(e['held']['sigma'])
    mu0=mp.mpf(float(e['est']['mu'])); A0=mp.mpf(float(e['est']['a']))
    star=(mp.mpf(e['star']['mu']),mp.mpf(e['star']['a']))
    f=lambda m,a: bhatt_ll(x,m,s,a)
    # coordinates: (mu, a) absolute; relative scale of mu is max(|mu|,sigma)
    g=[mp.diff(f,(mu0,A0),(1,0)),mp.diff(f,(mu0,A0),(0,1))]
    H=mp.matrix([[mp.diff(f,(mu0,A0),(2,0)),mp.diff(f,(mu0,A0),(1,1))],[mp.diff(f,(mu0,A0),(1,1)),mp.diff(f,(mu0,A0),(0,2))]])
    step=-(H**-1)*mp.matrix(g)
    print('score',[mp.nstr(v,5) for v in g]); print('newton step (mu,a)',[mp.nstr(v,6) for v in step])
    print('true error (mu,a)',mp.nstr(star[0]-mu0,6),mp.nstr(star[1]-A0,6))
    print('relative step a:',mp.nstr(step[1]/A0,5),' relative step mu/max(|mu|,sigma):',mp.nstr(step[0]/mu0,5))
    print('H eigen/cond',mp.nstr(mp.cond(H),5))
    json.dump(dict(score=[str(v) for v in g],step=[str(v) for v in step],err=[str(star[0]-mu0),str(star[1]-A0)],cond=str(mp.cond(H))),open('dev/research/evaluations/2026-11-15-refine-causes-data/newton_bhatt.json','w'))
