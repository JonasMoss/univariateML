#!/usr/bin/env python3
"""Independent 100-digit references for endpoint fits and critical-collapse bounds."""
from pathlib import Path
import subprocess,json,re
import mpmath as mp
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
boost=re.search(r'^Boost_INCLUDE_DIR[^=]*=(.*)$',(ROOT/'univariateML-cpp/build/CMakeCache.txt').read_text(),re.M)[1]
mp.mp.dps=100;records=[]
for mode in ['native','double']:
 exe=ROOT/'build/scratch'/('boundary-'+mode)
 command=['c++','-std=c++17','-O2','-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,str(HERE/'boundary-probe.cpp'),'-o',str(exe)]
 if mode=='double':command.insert(1,'-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS')
 subprocess.run(command,check=True)
 rows=[json.loads(s) for s in subprocess.check_output([str(exe)],text=True).splitlines()]
 for r in rows:
  name=r['name'];values=r['values'];passed=True
  if name=='fpareto-constant':passed=r['kind']=='nonexistence' and r['reason']=='unbounded'
  elif name in ['pareto-critical','gev-free-critical','gev-held-critical']:
   ref=mp.mpf(0) if name=='pareto-critical' else 3*mp.log(3)-6 if name=='gev-free-critical' else -4+6*mp.log(2)-3*mp.log(3)
   passed=r['kind']=='nonexistence' and r['reason']=='unattained' and mp.mpf(values[0])<=ref<=mp.mpf(values[1])
   r['reference']=str(ref)
  else:
   shape,scale,ll=values
   target=mp.mpf(2)/mp.log(2) if name=='shape-root' else mp.mpf(2)
   target_scale=mp.mpf('.5') if name=='scale-root' else mp.mpf(3) if name=='held-zero' else mp.mpf(1)
   data=[0,0,0] if name=='held-zero' else [0,1]
   density=mp.fsum(mp.log(mp.mpf(shape))-mp.log(mp.mpf(scale))-(mp.mpf(shape)+1)*mp.log1p(mp.mpf(x)/mp.mpf(scale)) for x in data)
   passed=r['kind']=='estimate' and abs(mp.mpf(shape)-target)<=mp.mpf('1e-10')*target and abs(mp.mpf(scale)-target_scale)<=mp.mpf('1e-10')*target_scale and abs(mp.mpf(ll)-density)<=mp.mpf('1e-10')*max(1,abs(density))
   r.update(reference_shape=str(target),reference_scale=str(target_scale),reference_loglik=str(density))
  r.update(mode=mode,passed=bool(passed));records.append(r)
(HERE/'boundary-references.json').write_text(json.dumps(records,indent=2)+'\n')
failed=[r for r in records if not r['passed']]
print(f'{len(records)} 100-digit boundary checks in both modes; {len(failed)} failures')
for r in failed:print(r)
raise SystemExit(bool(failed))
