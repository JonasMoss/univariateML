#!/usr/bin/env python3
"""Retain completed evidence only; refuses to finalize before green suites."""
import gzip,hashlib,json,re,shutil,subprocess,sys
from pathlib import Path
folder=Path(__file__).resolve().parent;root=folder.parents[3];scratch=root/'build/scratch/seam-rerun'
checks=['cpp-dev','cpp-no-long-double','r-dev','r-full','site-check']
records={}
native_retest=False
for name in checks:
 p=folder/f'{name}-run.json'
 assert p.exists(),f'Missing final run: {name}'
 records[name]=json.loads(p.read_text())
 if name=='cpp-dev' and records[name]['attempts'][-1]['exit']!=0:
  retest=folder/'cpp-dev-binomial-retest-run.json'
  assert retest.exists()
  followup=json.loads(retest.read_text());assert followup['attempts'][-1]['exit']==0
  log=(root/records[name]['attempts'][-1]['log']).read_text()
  assert re.findall(r'\d+ - (\S+) \(Failed\)',log)==['distribution_binom']
  records['cpp-dev-binomial-retest']=followup;native_retest=True
 else:assert records[name]['attempts'][-1]['exit']==0,(name,records[name])
for name in ('cpp-no-long-double-local','cpp-binomial-double-retest'):
 p=folder/f'{name}-run.json'
 if p.exists():records[name]=json.loads(p.read_text());assert records[name]['attempts'][-1]['exit']==0
subprocess.run([sys.executable,str(folder/'analyze.py')],cwd=root,check=True)
subprocess.run([sys.executable,str(folder/'policy-check.py')],cwd=root,check=True)
analysis=json.loads((folder/'analysis.json').read_text())
for mode in ('native','double'):
 assert not analysis[mode]['parameters_or_terminal_status_changes'],mode
 assert not analysis[mode]['stop_reason_changes'],mode
 assert not analysis[mode]['outside_support'],mode
assert len(analysis['native']['remaining'])==0
assert [(r['family'],r['index']) for r in analysis['double']['remaining']]==[('bnbinom',579)]
assert [(r['family'],r['index']) for r in analysis['double']['open']]==[('emg',1311)]
assert not analysis['native']['open']
import mpmath as mp
import struct
mp.mp.dps=250
final_points={}
for mode in ('native','double'):
 with gzip.open(folder/f'final-{mode}.jsonl.gz','rt') as stream:
  final_points[mode]={(r['family'],r['source'],r['index']):r for r in map(json.loads,stream)}
verification=[]
bits=lambda values:[struct.pack('!d',v).hex() for v in values]
for before in json.loads((folder/'high-precision.json').read_text()):
 after=final_points[before['mode']][(before['family'],before['source'],before['index'])]
 assert bits(before['parameters'])==bits(after['parameters'])
 assert bits(before['data'])==bits(after['data'])
 reference=mp.mpf(before['high_precision'][-1]['sum'])
 before_error=abs(mp.mpf(before['reported'])-reference)/max(1,abs(reference))
 after_error=abs(mp.mpf(after['reported'])-reference)/max(1,abs(reference))
 if before['family']!='bnbinom':assert after_error<=mp.mpf('1e-10')
 verification.append(dict(mode=before['mode'],family=before['family'],index=before['index'],parameters_bits_unchanged=True,data_bits_unchanged=True,reference_digits=250,reference=str(reference),before_reported=before['reported'],after_reported=after['reported'],before_relative_error=str(before_error),after_relative_error=str(after_error)))
(folder/'returned-point-verification.json').write_text(json.dumps(verification,indent=2)+'\n')
logs=[]
for p in sorted(scratch.iterdir()):
 if p.suffix=='.jsonl':
  name=p.name.replace('owned-before-native','focused-after-native')
  with p.open('rb') as src,gzip.open(folder/(name+'.gz'),'wb') as dest:shutil.copyfileobj(src,dest)
 elif p.name.endswith(('.provenance.json','.summary.json')):
  shutil.copyfile(p,folder/p.name.replace('owned-before-native','focused-after-native'))
 elif p.suffix=='.log':
  target=folder/(p.name.replace('owned-before-native','focused-after-native')+'.gz')
  with p.open('rb') as src,gzip.open(target,'wb') as dest:shutil.copyfileobj(src,dest)
  logs.append(dict(original=str(p.relative_to(root)),retained=target.name))
(folder/'log-index.json').write_text(json.dumps(logs,indent=2)+'\n')
validation={}
for name,record in records.items():
 attempt=record['attempts'][-1];text=(root/attempt['log']).read_text()
 cpp=re.findall(r'100% tests passed, 0 tests failed out of (\d+)',text)
 r=re.findall(r'\[\s*FAIL\s+[^\n\]]+\]',text)
 validation[name]=dict(machine=attempt['machine'],exit=attempt['exit'],cpp_shards=int(cpp[-1]) if cpp else None,r_summary=r[-1] if r else None,attempts=record['attempts'],retained_log=Path(attempt['log']).name+'.gz')
 if name=='cpp-dev' and native_retest:
  total=int(re.findall(r'tests failed out of (\d+)',text)[-1])
  validation[name].update(cpp_shards=total,initial_shards_passed=total-1,final_coverage='pass after family-only binomial retest',retest_record='cpp-dev-binomial-retest-run.json')
(folder/'validation-summary.json').write_text(json.dumps(validation,indent=2)+'\n')
index=[]
for p in sorted(folder.iterdir()):
 if not p.is_file() or p.name=='evidence-index.json':continue
 index.append(dict(file=p.name,bytes=p.stat().st_size,sha256=hashlib.sha256(p.read_bytes()).hexdigest()))
(folder/'evidence-index.json').write_text(json.dumps(index,indent=2)+'\n')
print(json.dumps(validation,indent=2))
print('Retained evidence files:',len(index))
