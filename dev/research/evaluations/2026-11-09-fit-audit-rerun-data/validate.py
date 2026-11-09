#!/usr/bin/env python3
"""Check census completeness, case identity, and independent simple oracles."""
import os,collections,hashlib,json,pathlib,platform,re,subprocess,sys
import mpmath as mp
HERE=pathlib.Path(__file__).resolve().parent;ROOT=HERE.parents[3]
sys.path.insert(0,str(HERE));import audit
cases=json.loads((HERE/'cases.json').read_text());refs=list(map(json.loads,(HERE/os.environ.get('REFERENCE_FILE','reference.jsonl')).read_text().splitlines()))
fits=[l.split(',') for l in (HERE/'fits.csv').read_text().splitlines()]
assert len(cases)==len(refs)==len(fits)
harness=(ROOT/'build/scratch/fit-audit/stress.cpp').read_text()
frozen_holds_checked=0
for folder,m,g in audit.models():
 block=harness.split('if(family=="'+m['id']+'")',1)[1].split('if(family==',1)[0]
 values=re.findall(r'audit::Held\{bool\(mask&(\d+)\),([^}]+)\}',block)
 assert len(values)==len(m['params']),(m['id'],'generated hold count')
 for i,((bit,value),p) in enumerate(zip(values,m['params'])):
  assert int(bit)==1<<i
  actual=float(value.replace('INFINITY','inf'))
  expected=float(g['estimates'].get(p['name'],p.get('simulate',1)))
  assert actual==expected,(m['id'],p['name'],actual,expected)
  frozen_holds_checked+=1

assert set(range(len(cases)))=={r['index'] for r in refs}=={int(f[2]) for f in fits}
assert len({r['index'] for r in refs})==len(refs)
assert {m['id'] for _,m,_ in audit.models()}=={c['family'] for c in cases}
assert all(cases[r['index']]['family']==r['family'] for r in refs)
fit_by_id={int(f[2]):f for f in fits}
assert all(int(fit_by_id[r['index']][3])==r['status'] for r in refs),'reference/core status mismatch after retries'
assert {p.parent.name for p in (audit.DISTS).glob('*/mle.hpp')}=={c['family'] for c in cases}
assert all(r.get('coefficient_error_verified') for r in refs if r['classification']=='silent failure' and r.get('evidence','').startswith('stationary'))
assert all('precision_check' in r or 'support_exclusion' in r or r.get('evidence')=='status Ok with nonfinite reported logLik' for r in refs if r['classification']=='silent failure')
mp.mp.dps=100
meta={m['id']:m for _,m,_ in audit.models()}
root_proxies=[]
for r in refs:
 if not r.get('coefficient_error_verified'):continue
 c=cases[r['index']];ps=meta[r['family']]['params'];t=list(map(mp.mpf,r['theta']));base=list(map(mp.mpf,r['refined_theta']));data=list(map(mp.mpf,c['data']))
 free=[i for i,p in enumerate(ps) if p['name'] in r.get('scores',{})]
 ratios=[]
 for i in free:
  signed=ps[i]['domain'].split(',')[0].lstrip('([').strip().startswith('-')
  denominator=max(1,abs(base[i])) if signed else max(abs(base[i]),mp.mpf('1e-300'))
  ratios.append(audit.coordinate_unit(r['family'],ps,t,data,i)/denominator)
 proxy=mp.sqrt(len(free))*mp.mpf(r['stationary_residual_per_observation'])/mp.mpf(r['local_information_eigenvalue_range'][0])*max(ratios)
 assert proxy<min(mp.mpf('1e-10'),mp.mpf(r['parameter_relative_error'])/100),('reference root uncertainty needs further refinement',r['index'],str(proxy))
 root_proxies.append(dict(index=r['index'],linearized_relative_root_error_proxy=str(proxy)))
(HERE/'root-precision-check.json').write_text(json.dumps(dict(validation='PASS',interpretation='Residual / local minimum information eigenvalue, converted to relative coefficient units. A numerical local consistency check, not a rigorous curvature bound.',checks=root_proxies),indent=2))
mp.mp.dps=70
checked=0
for r in refs:
 c=cases[r['index']]
 if r['family'] not in ['norm','exp','pois'] or 'reference_logLik' not in r:continue
 mp.mp.dps=r.get('precision_digits',70)
 p=list(map(mp.mpf,r['theta']));x=list(map(mp.mpf,c['data']))
 if r['family']=='norm':manual=mp.fsum(-mp.log(p[1])-mp.log(2*mp.pi)/2-(v-p[0])**2/(2*p[1]**2) for v in x)
 elif r['family']=='exp':manual=len(x)*mp.log(p[0])-p[0]*mp.fsum(x)
 else:manual=mp.fsum(v*mp.log(p[0])-p[0]-mp.loggamma(v+1) for v in x)
 assert abs(manual-mp.mpf(r['reference_logLik']))<=mp.mpf('1e-60')*max(1,abs(manual))
 checked+=1
files=[ROOT/'dev/validation/diagnostics.py',ROOT/'dev/validation/quality/density.py']
files+=sorted((ROOT/'univariateML-cpp/include').rglob('*.hpp'))
files+=sorted((ROOT/'bindings/common').rglob('*.hpp'))
files+=sorted((ROOT/'univariateML-cpp/include/cppunivariate/distributions').glob('*/golden.json'))
files+=sorted((ROOT/'univariateML-cpp/include/cppunivariate/distributions').glob('*/README.md'))
(HERE/'source-sha256.json').write_text(json.dumps({str(f.relative_to(ROOT)):hashlib.sha256(f.read_bytes()).hexdigest() for f in files},indent=2))
result=dict(frozen_held_parameter_literals_checked=frozen_holds_checked,cases=len(cases),families=len({c['family'] for c in cases}),subsets=len({(c['family'],c['mask']) for c in cases}),independent_simple_likelihood_checks=checked,classifications=dict(collections.Counter(r['classification'] for r in refs)),validation='PASS',native_census_machine='workstation',validator_host=platform.node())
(HERE/'validation.json').write_text(json.dumps(result,indent=2));print(json.dumps(result,indent=2))
