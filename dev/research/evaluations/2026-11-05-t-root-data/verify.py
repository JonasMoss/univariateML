"""Independent high-precision checks of root enclosures and Hessian formulas."""
import json
from pathlib import Path
import mpmath as mp
import numpy as np
import roots
from fractions import Fraction

HERE=Path(__file__).resolve().parent
mp.mp.dps=80
cases=json.loads((roots.OLD/'inputs.json').read_text())
rows=json.loads((HERE/'roots.json').read_text())
global_rows=json.loads((HERE/'global.json').read_text())
for row in global_rows:
    iso=row.get('isolation')
    if iso and iso.get('outcome')=='root_and_tube' and not any(q['name']==row['name'] and q['family']==row['family'] for q in rows):
        rows.append({'name':row['name'],'family':row['family'],**iso})
checks=[];goldens=[]
for row in rows:
    if row['outcome'] not in ['root_and_tube','root_only']:continue
    case=next(q for q in cases if q['name']==row['name'])
    center=mp.mpf(row['center']);unit=mp.mpf(row['unit']);v0=mp.mpf(row['v0'])
    x=[(mp.mpf(q)-center)/unit for q in case['x']];n=len(x)
    def cost(m,t,w):
        v=w*v0;a=mp.exp(t)
        c=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(mp.pi)/2
        return -n*c-n*v*t+(v+1)/2*mp.fsum(mp.log((q-m)**2+a*a) for q in x)
    def g(m,t,w):
        p=(m,t,w)
        return tuple(mp.diff(lambda z:cost(*(p[:j]+(z,)+p[j+1:])),p[j]) for j in range(3))
    proposal=tuple(map(mp.mpf,row['point']))
    p=mp.findroot(g,proposal,tol=mp.mpf('1e-65'),maxsteps=50)
    lo=list(map(mp.mpf,row['box']['lo']));hi=list(map(mp.mpf,row['box']['hi']))
    assert all(l<q<h for l,q,h in zip(lo,p,hi)),row['name']
    # Finite differentiation is independent of the analytic Hessian code.
    Hmp=mp.matrix([[mp.diff(lambda z:g(*(tuple(p[:j])+(z,)+tuple(p[j+1:])))[i],p[j]) for j in range(3)] for i in range(3)])
    Hfloat=roots.equations(np.array(list(map(float,x))),np.array(list(map(float,p))),float(v0))[1]
    err=max(float(abs(Hmp[i,j]-Hfloat[i,j])/(1+abs(Hmp[i,j]))) for i in range(3) for j in range(3))
    assert err<2e-10,(row['name'],err)
    v=p[2]*v0;m=center+unit*p[0];s=unit*mp.exp(p[1])/mp.sqrt(v)
    values={'mu':m,'scale':s,'nu':v}
    if v>2:values['sd']=s/mp.sqrt(1-2/v)
    for key,q in values.items():
        assert mp.mpf(row['coefficients'][key][0])<q<mp.mpf(row['coefficients'][key][1]),(row['name'],key)
    checks.append({'name':row['name'],'family':row['family'],'hessian_relative_error':err,
                   'score_norm':str(mp.norm(g(*p))),'root':[str(q) for q in p],
                   'coefficients':{k:str(q) for k,q in values.items()}})
    if row['name']==row['family']+'-golden':
        family=row['family'];repo=HERE.parents[3]
        frozen=json.loads((repo/f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())['estimates']
        pairs=[('mu','mu'),('scale','sigma'),('nu','df')] if family=='lst' else [('mu','mean'),('sd','sd'),('nu','nu')]
        discrepancy=max(abs(mp.mpf(endpoint)-frozen[public])/abs(frozen[public]) for key,public in pairs for endpoint in row['coefficients'][key])
        assert discrepancy<mp.mpf('3e-5'),(family,discrepancy)
        goldens.append({'family':family,'whole_box_max_relative_golden_error':str(discrepancy),'tolerance':3e-5})

# Exact equality endpoint identity and the two-point profile formula.
faces=[]
for floor,data in [(1,[0,0,-1,1]),(1,[0,0,1,1]),(2,[0,0,0,0,-1,1])]:
    q=mp.mpf(floor);n=len(data);k=data.count(0)
    c=mp.loggamma((q+1)/2)-mp.loggamma(q/2)-mp.log(mp.pi)/2
    endpoint=n*c-(q+1)*mp.fsum(mp.log(abs(v)) for v in data if v!=0)
    errors=[]
    for a in [mp.mpf('1e-4'),mp.mpf('1e-8')]:
        val=n*c+n*q*mp.log(a)-(q+1)/2*mp.fsum(mp.log(mp.mpf(v)**2+a*a) for v in data)
        assert val<endpoint
        errors.append(float(endpoint-val))
    assert errors[1]<errors[0]/mp.mpf('1e7')
    faces.append({'floor':floor,'x':data,'collapse_value':str(endpoint),'approach_errors':errors})
two=[]
for v in map(mp.mpf,[1,2,3,10,1000]):
    slope=(mp.digamma((v+1)/2)-mp.digamma(v/2)-mp.log1p(1/v))/2
    assert slope>0
    two.append({'nu':str(v),'slope_per_observation':str(slope)})

# Invalid arithmetic and budget refusal are intentional tested outcomes.
refusals={}
for name,operation in [('denominator',lambda:roots.I(1)/roots.I(-1,1)),('log',lambda:roots.I(-1,1).log())]:
    try:operation();raise AssertionError('unguarded domain')
    except ArithmeticError:refusals[name]='ArithmeticError'

endpoints=[]
for row in global_rows:
    if row['outcome'] not in ['cauchy','t2']:continue
    case=next(q for q in cases if q['name']==row['name']);box=row['endpoint_box']
    center=mp.mpf(box['coordinate_center']);unit=mp.mpf(box['coordinate_unit']);v=mp.mpf(box['nu'])
    x=[(mp.mpf(q)-center)/unit for q in case['x']];n=len(x)
    def gs(m,t):
        rr=[q-m for q in x];a2=mp.exp(2*t);dd=[q*q+a2 for q in rr]
        return (-(v+1)*mp.fsum(r/d for r,d in zip(rr,dd)),n-(v+1)*mp.fsum(r*r/d for r,d in zip(rr,dd)))
    start=tuple((mp.mpf(l)+mp.mpf(h))/2 for l,h in zip(box['lo'],box['hi']))
    p=mp.findroot(gs,start,tol=mp.mpf('1e-65'))
    assert all(mp.mpf(l)<q<mp.mpf(h) for l,q,h in zip(box['lo'],p,box['hi']))
    c=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(mp.pi)/2
    value=n*c+n*v*p[1]-(v+1)/2*mp.fsum(mp.log((q-p[0])**2+mp.exp(2*p[1])) for q in x)
    normal=-n*(mp.log(2*mp.pi)/2+mp.mpf('.5'))
    assert value-normal>=mp.mpf(row['witness'])
    endpoints.append({'name':row['name'],'family':row['family'],'nu':str(v),
                       'score_norm':str(mp.norm(gs(*p))),'mu':str(center+unit*p[0]),
                       'ordinary_scale':str(unit*mp.exp(p[1])/mp.sqrt(v))})
result={'precision_digits':80,'roots_checked':checks,'golden_boxes':goldens,'endpoint_roots_checked':endpoints,
        'face_checks':faces,'balanced_two_point_slopes':two,'guard_checks':refusals}
data=[0,0,0,0,-1,1];n=len(data)
v=Fraction(sum(q*q for q in data),n)
m4=Fraction(sum(q**4 for q in data),n)/v**2
m6=Fraction(sum(q**6 for q in data),n)/v**3
assert m4==3 and m6==9
critical=[]
for nu in map(mp.mpf,['2.000001','2.01','3','10','1000']):
    slope=(mp.digamma((nu+1)/2)-mp.digamma(nu/2))/2-mp.log((nu+1)/(nu-2))/6
    assert slope<0
    critical.append({'nu':str(nu),'slope_per_observation':str(slope)})
result['zero_slope_critical_case']={'A_exact':'0','B_exact':str(Fraction(n,4)),'slopes':critical}
decided=0
for row in global_rows:
    outcome=row['outcome'];case=next(q for q in cases if q['name']==row['name'])
    floor=1 if row['family']=='lst' else 2
    if outcome in ['finite','cauchy','t2','normal','collapse','collapse_unbounded']:decided+=1
    if outcome=='finite':
        assert row['isolation']['outcome']=='root_and_tube'
        assert row['isolation']['coefficients']['nu'][0]>floor
        assert mp.mpf(row['tail']['upper'])<=mp.mpf(row['witness'])
    if outcome in ['cauchy','t2']:
        assert row['endpoint_box'] and row['window']['method']=='relative_endpoint_chord'
        assert mp.mpf(row['window']['slope_upper'])<0
        assert mp.mpf(row['tail']['upper'])<=mp.mpf(row['witness'])
    if outcome=='normal' and row.get('profiles',0)>0:
        assert row['tail']['kind']=='signed' and mp.mpf(row['tail']['A'])<0
        assert mp.mpf(row['tail']['inward_margin'])>0 and mp.mpf(row['tail']['slope_margin'])>0
    if outcome=='collapse_unbounded':
        k=max(case['x'].count(q) for q in case['x']);assert k>floor*(len(case['x'])-k)
assert len(global_rows)==62 and decided>=60
result['global_logic_checks']={'pairs':len(global_rows),'decided':decided,'unresolved':62-decided}
# Run the public research guard with deliberate zero/one profile budgets.
import importlib
engine=importlib.import_module('global')
budgets={}
for budget in [0,1]:
    q=engine.Engine(cases[0]['x'],1,budget).solve()
    assert q['outcome']=='budget' and 'coefficients' not in q
    budgets[str(budget)]=q
result['budget_checks']=budgets
(HERE/'independent.json').write_text(json.dumps(result,indent=2,allow_nan=False)+'\n')
print('Independent root, whole golden box, Hessian and lower-face checks passed:',len(checks),flush=True)
