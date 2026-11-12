from pathlib import Path
import json,subprocess,sys,collections
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
OLD=HERE.parent/'2026-11-10-fallback-cont-data'
if sys.argv[1]=='init':
 cases=[]
 for prefix in ['', 'delegate-']:
  rows=[s.split('\t') for s in (OLD/(prefix+'after.tsv')).read_text().splitlines()]
  ids={r[0] for r in rows if r[2] in ['certified','uncertified']}
  for c in json.loads((OLD/(prefix+'cases.json')).read_text()):
   if str(c['index']) in ids and c['family'] not in ['sged','invpareto','pareto2','trgamma']:
    c['index']=prefix+str(c['index']);c['tol']=1e-10;cases.append(c)
 (HERE/'cases.json').write_text(json.dumps(cases,indent=2)+'\n')
 exit()
mode=sys.argv[1]; double='double' in mode
binary=ROOT/'build/scratch'/('acc-'+mode)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
subprocess.run(['g++','-O2','-std=c++17']+(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if double else [])+['-I'+str(ROOT/('build/scratch/acc-baseline/univariateML-cpp/include' if 'before' in mode else 'univariateML-cpp/include')),'-I'+str(ROOT/'bindings'),'-I'+boost,str(HERE/'runner.cpp'),'-o',str(binary)],check=True)
cases=json.loads((HERE/'cases.json').read_text());lines=[]
if mode.startswith('bhatt-current'):
 cases=[c for c in cases if c['family']=='bhatt' and str(c['index']).startswith('census')]
if mode.startswith('strict'):
 cases=[c for c in cases if str(c['index']).startswith('strict')]
if mode.startswith('census'):
 cases=[c for c in cases if str(c['index']).startswith('census') or c['family']=='sn']
if mode.startswith('probe'):
 ids={r[0] for r in [l.split('\t') for l in (HERE/'before-native.tsv').read_text().splitlines()] if r[2]=='uncertified'}
 cases=[c for c in cases if str(c['index']) in ids or str(c['index']).startswith('strict')]
for c in cases:
 held={k:v for k,v in c['held'].items() if v is not None}
 values=[c['family'],str(c['index']),str(len(c['data'])),str(len(held)),str(c.get('iterlim',1000)),str(c['tol'])]+list(map(repr,c['data']))
 for k,v in held.items():values += [k,repr(v)]
 lines.append(' '.join(values))
with (HERE/(mode+'.tsv')).open('w') as f:subprocess.run([str(binary)],input='\n'.join(lines)+'\n',stdout=f,text=True,check=True)
print(collections.Counter(line.split('\t')[2] for line in (HERE/(mode+'.tsv')).read_text().splitlines()))
