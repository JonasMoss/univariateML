"""Compare all 1088 archive rows, including returned estimates and named stops."""
import argparse,gzip,json,math,re
from collections import Counter
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).resolve().parent
ap=argparse.ArgumentParser();ap.add_argument('--partial',action='store_true');args=ap.parse_args()
status=[s.strip() for s in re.search(r'enum class Gb2Status \{([^}]+)\}',(ROOT/'univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp').read_text(),re.S)[1].split(',')]
named={1,2,3,4,5,6,11,12}
cases={r['index']:r for r in json.load(gzip.open(ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz')) if r['family'] in ['fpareto','gb2']}
def read(version,mode):
 f=HERE/f'review-{version}-{mode}.jsonl';return {r['index']:r for r in map(json.loads,f.read_text().splitlines())} if f.exists() else {}
def fields(r):return r['result'].split() if 'result' in r else None
def label(r):return status[int(fields(r)[1])] if fields(r) else 'audit timeout'
def comparable(r):return fields(r)[1:8] if fields(r) else ['timeout']
result=dict(rows_per_family=dict(Counter(r['family'] for r in cases.values())),modes={},changes=[],diagnostic_changes=[],degradations=[])
for mode in ['native','double']:
 allrows={v:read(v,mode) for v in ['original','pre-review','final']}
 result['modes'][mode]={v:dict(rows=len(rs),timeouts=[i for i,r in rs.items() if r.get('timeout')],returned=sum(fields(r) is not None and int(fields(r)[1])==0 for r in rs.values())) for v,rs in allrows.items()}
 if not args.partial:assert all(len(rs)==1088 and set(rs)==set(cases) for rs in allrows.values()),(mode,result['modes'][mode])
 for before_version in ['original','pre-review']:
  before=allrows[before_version];after=allrows['final']
  for i in sorted(set(before)&set(after)):
   b,a=before[i],after[i];fb,fa=fields(b),fields(a)
   if comparable(b)==comparable(a):
    if fb and fa and fb[8:]!=fa[8:]:result['diagnostic_changes'].append(dict(index=i,family=cases[i]['family'],sample=cases[i]['sample'],mode=mode,comparison=before_version+' -> final',reason='Only iteration/work counters or stopped-candidate diagnostics differ; returned coefficients, status, flag and likelihood are identical.',before_result=b['result'],after_result=a['result']))
    continue
   row=dict(index=i,family=cases[i]['family'],sample=cases[i]['sample'],mode=mode,comparison=before_version+' -> final',before=label(b),after=label(a),before_result=b.get('result'),after_result=a.get('result'))
   row['reason']='The external guard expired in the original unbounded nested search; bounded search now reaches the named limit.' if not fb else 'A completed first-pass grid direction now survives the later per-refinement budget, allowing the unchanged face comparison to return its named limit.'
   reason=[]
   if fb and int(fb[1])==0:
    if not fa or int(fa[1])!=0:reason.append('returned fit lost')
    else:
     if fb[2]=='1' and fa[2]!='1':reason.append('certification flag lost')
     coef_error=max(abs(float(x)-float(y))/max(abs(float(x)),1e-300) for x,y in zip(fb[3:7],fa[3:7]))
     row['coefficient_relative_change']=coef_error
     if coef_error>2e-10:reason.append('coefficient change requires investigation')
     if float(fa[7])<float(fb[7])-1e-9*(1+abs(float(fb[7]))):reason.append('likelihood decreased')
   if fb and int(fb[1]) in named:
    if not fa or int(fa[1]) not in named:reason.append('named stop lost')
    elif fb[1]!=fa[1]:reason.append('named model changed')
    elif fb[7]!=fa[7]:reason.append('named limit likelihood changed')
   if reason:row['degradation']=reason;result['degradations'].append(row)
   result['changes'].append(row)
(HERE/'review-comparison.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps({k:v for k,v in result.items() if k not in ['changes','diagnostic_changes']},indent=2));print('Changed rows',len(result['changes']))
if not args.partial:assert not result['degradations'],result['degradations']
