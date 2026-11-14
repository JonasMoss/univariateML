#!/usr/bin/env python3
"""Keep final snapshots and workstation suite logs beside the report."""
import gzip,json,shutil
from pathlib import Path
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
scratch=ROOT/'build/scratch'
for mode in ('native','double','routes-native','routes-double'):
 source=ROOT/'build/snap'/mode
 shutil.copyfile(source/'snapshot.jsonl.gz',HERE/(mode+'.jsonl.gz'))
 shutil.copyfile(source/'provenance.json',HERE/(mode+'-provenance.json'))
 logs=scratch/('snapshot-'+mode+'.log')
 with logs.open('rb') as src,gzip.open(HERE/('snapshot-'+mode+'.log.gz'),'wb') as dst:
  shutil.copyfileobj(src,dst)
suites=[('cpp-test','cpp-test.log',0),('cpp-test-no-long-double','cpp-test-no-long-double.log',0)]
for line in (scratch/'post-holds-suites.tsv').read_text().splitlines():
 name,code,machine=line.split('\t');assert machine=='workstation'
 suites.append((name,name+'-post-holds.log',int(code)))
records=[]
for name,filename,code in suites:
 target=HERE/(name+'.log.gz')
 with (scratch/filename).open('rb') as src,gzip.open(target,'wb') as dst:
  shutil.copyfileobj(src,dst)
 records.append(dict(command='just '+name,exit_code=code,machine='workstation',log=target.name))
(HERE/'suites.json').write_text(json.dumps(records,indent=1)+'\n')
provenance=[json.loads((HERE/(m+'-provenance.json')).read_text()) for m in ('native','double','routes-native','routes-double')]
assert len({p['source_sha256'] for p in provenance})==1,'snapshot source fingerprints differ'
print('Collected final snapshots and',len(records),'suite logs; source',provenance[0]['source_sha256'])
