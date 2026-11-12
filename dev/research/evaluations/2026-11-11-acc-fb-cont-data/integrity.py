from pathlib import Path
import subprocess,json,hashlib
P=Path(__file__).resolve().parent;ROOT=P.parents[3]
base='bc8ff7be2e3bc4b9186a56fc4f2e8ee45c163714'
owned=set('burr invburr pareto4 paralogis invparalogis betapr gb2 fpareto pareto3 ged genbeta bhatt sn'.split())
changed=subprocess.check_output(['git','diff','--name-only'],text=True).splitlines()
new=subprocess.check_output(['git','ls-files','--others','--exclude-standard'],text=True).splitlines()
errors=[]
for name in changed+new:
 parts=Path(name).parts
 allowed=(name.startswith(str(P.relative_to(ROOT))) or name=='dev/research/evaluations/2026-11-11-acc-fb-cont-report.md' or len(parts)>5 and parts[:4]==('univariateML-cpp','include','cppunivariate','distributions') and parts[4] in owned)
 if not allowed:errors.append('outside scope: '+name)
 if Path(name).name in ['dpqr.hpp','truth.py','truth.json','golden.json']:errors.append('settled source changed: '+name)
for name in changed:
 current=(ROOT/name).read_bytes();old=subprocess.check_output(['git','show',base+':'+name])
 if name.endswith('/test.cpp') and not current.startswith(old):errors.append('existing test changed: '+name)
 if name.endswith('/README.md') and current.split(b'---',2)[:2]!=old.split(b'---',2)[:2]:errors.append('front matter changed: '+name)
r=dict(baseline=base,changed=changed,new=new,errors=errors,sha256={name:hashlib.sha256((ROOT/name).read_bytes()).hexdigest() for name in changed+new if (ROOT/name).is_file() and not name.startswith(str(P.relative_to(ROOT)))})
(P/'source-integrity.json').write_text(json.dumps(r,indent=2)+'\n');print(json.dumps(errors));assert not errors
