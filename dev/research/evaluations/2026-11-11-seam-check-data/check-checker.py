"""Independent elementary checks of the audit's exact-support and error gates."""
import importlib.util,json,math,sys
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
sys.path.insert(0,str(ROOT/'dev/validation'))
spec=importlib.util.spec_from_file_location('seam',ROOT/'dev/validation/seam-check.py')
s=importlib.util.module_from_spec(spec);spec.loader.exec_module(s)
checks=[]
for ident,pars,data,want in [
    ('gev',[0.,1.,-3.],[1/3,math.nextafter(1/3,math.inf)],[1]),
    ('gpd',[0.,1.,-1.],[0.,1.,math.nextafter(1.,math.inf)],[2]),
    ('triang',[0.,1.,.5],[0.,.5,1.,math.nextafter(0.,-math.inf)],[3]),
    ('tpois',[1.,0.,3.],[0.,1.,3.,4.],[0,3]),
    ('nsbeta',[2.,3.,-1.,1.],[-1.,0.,1.],[]),
]:
    m,_=s.read(s.DISTS/ident)
    actual=s.support_violations(m,pars,data)
    assert actual==want,(ident,actual,want)
    checks.append(dict(family=ident,parameters=pars,data=data,outside=actual))
assert s.error(1.,1.)==0
assert s.error(math.nan,1.)==math.inf
assert s.error(-math.inf,-math.inf)==0
assert s.error(1.,2.)==.5
out=dict(exact_support=checks,error_gate_checks=4)
Path(__file__).with_suffix('.json').write_text(json.dumps(out,indent=2)+'\n')
print('5 exact-support probes and 4 error-gate probes passed')
