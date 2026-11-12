"""Archive preservation and scope evidence; result columns are documented in driver.cpp."""
import gzip,hashlib,json,re,subprocess
from collections import Counter
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
read=lambda name:{r['index']:r for r in map(json.loads,(HERE/name).read_text().splitlines())}
b=read('default-before.jsonl');a=read('final-native.jsonl');d=read('final-double.jsonl')
assert len(a)==len(b)==len(d)==544
assert not any(r.get('timeout') for r in [*a.values(),*d.values()])
cases=json.loads((HERE/'cases.json').read_text());archive=[r for r in json.load(gzip.open(ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz')) if r['family']=='fpareto'];assert cases==archive

def preserved(before,after):
 ok=[i for i in before if before[i].get('result','').split()[1:2]==['0']]
 changed=[i for i in ok if before[i]['result'].split()[:8]!=after[i].get('result','').split()[:8]]
 assert not changed,changed
 return dict(returned=len(ok),certified=sum(before[i]['result'].split()[2]=='1' for i in ok),changed=changed)
summary=dict(baseline='bb46078757dad3538bfb263086d4e72cde163330',cases=len(a),input_identity=True,default=preserved(b,a),strict=preserved(read('strict-before.jsonl'),read('strict-after.jsonl')),baseline_timeouts=[i for i in b if b[i].get('timeout')])
summary['changed_stops']=[dict(index=i,before=b[i],after=a[i]) for i in b if b[i].get('result','').split()[1:8]!=a[i].get('result','').split()[1:8]]
summary['target']={i:dict(before=b[i],native=a[i],double=d[i]) for i in [1599,1600,10717]}
enum=(ROOT/'univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp').read_text()
names=[s.strip() for s in re.search(r'enum class Gb2Status \{([^}]+)\}',enum,re.S)[1].split(',')]
summary['changed_stop_names']=[dict(index=r['index'],before='audit timeout' if r['before'].get('timeout') else names[int(r['before']['result'].split()[1])],after=names[int(r['after']['result'].split()[1])]) for r in summary['changed_stops']]
summary['profiles']={}
for i in [1599,1600,10717]:
 line=next(l for l in (HERE/f'profile-{i}.txt').read_text().splitlines() if l.endswith('cppunivariate::gb2_detail::evaluate(cppunivariate::trgamma_detail::LogData const&, double, double, double, double, bool, double)'))
 fields=line.split();summary['profiles'][i]=dict(sampled_cpu_percent=float(fields[0]),eight_second_evaluations=int(fields[3]))
summary['compiler']=subprocess.check_output(['g++','--version'],text=True).splitlines()[0]
summary['files']={str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for family in ['gb2','fpareto'] for name in ['README.md','mle.hpp','test.cpp'] for p in [ROOT/'univariateML-cpp/include/cppunivariate/distributions'/family/name]}
(HERE/'comparison.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps({k:v for k,v in summary.items() if k not in ['files','changed_stops','target']},indent=2))
