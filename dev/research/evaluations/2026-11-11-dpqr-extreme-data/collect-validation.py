"""Keep final logs and exact command outcomes after foreground runs complete."""
import gzip,json,re
from pathlib import Path
H=Path(__file__).resolve().parent;ROOT=H.parents[3];out=H/'final-logs';out.mkdir(exist_ok=True)
native=H/'native-final.tsv'
if native.exists():
 rows=(H/'local-final-runs.tsv').read_text().splitlines();line=native.read_text().strip();rows=[line if x.startswith('cpp-test\t') else x for x in rows];(H/'local-final-runs.tsv').write_text('\n'.join(rows)+'\n')
records=[]
for receipt,where,logs in [('local-final-runs.tsv','workstation',ROOT/'build/scratch/final-runs'),('remote-final-runs.tsv','simbox',ROOT/'build/scratch/ultimate-remote-logs')]:
 for line in (H/receipt).read_text().splitlines():
  recipe,machine,code=line.split('\t');assert machine==where
  data=(logs/(recipe+'.log')).read_text();r=dict(command='just '+recipe,machine=machine,exit=int(code))
  clean=re.sub(r'\x1b\[[0-9;]*[A-Za-z]','',data)
  if recipe in ('test','test-full'):
   matches=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',clean)
   if matches:r['counts']=dict(zip(('fail','warn','skip','pass'),map(int,matches[-1])))
   r['failure_locations']=sorted(set(re.findall(r"Failure \('([^']+)'\)",clean)))
  if recipe=='js-test':
   r['counts']={k:int(v) for k,v in re.findall(r'^# (tests|pass|fail|cancelled|skipped|todo) (\d+)$',clean,re.M)}
  if recipe.startswith('cpp-test'):
   counts=re.findall(r'(\d+)% tests passed, (\d+) tests failed out of (\d+)',clean)
   assert counts,(recipe,machine)
   pc,n,total=map(int,counts[-1]);r['counts']=dict(total=total,failed=n,passed=total-n)
   footer=clean.rsplit('The following tests FAILED:',1)[-1] if n else ''
   r['failed_shards']=re.findall(r'\d+ - (\w+) \(Failed\)',footer)
   r['assertion_failure_lines']=sorted(set(re.findall(r'[^\n]*ERROR: CHECK[^\n]*',clean)))
  gz=out/(machine+'-'+recipe+'.log.gz')
  gz.write_bytes(gzip.compress(data.encode(),mtime=0));r['log']=str(gz.relative_to(H));records.append(r)
(H/'final-validation.json').write_text(json.dumps(records,indent=2)+'\n')
for r in records:print(r['command'],r['machine'],r['exit'],r.get('counts'),r.get('failed_shards',r.get('failure_locations')))
