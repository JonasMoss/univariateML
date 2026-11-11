"""Replay frozen count cases; run each binary under a workstation CPU slot.
Build study.cpp from baseline bc8ff7be or current headers, no archived trees.
"""
import json,pathlib,subprocess,sys,os
HERE=pathlib.Path(__file__).resolve().parent
KEYS=['size','alpha','beta','m','N','k','r','prob','pi','p0','a','b','mu1','mu2']
cs=json.loads((HERE/'cases.json').read_text())
if '--uncertified' in sys.argv:
 old=HERE.parent/'2026-11-10-fallback-count-data/after-final.tsv'
 ids={int(l.split('\t')[0]) for l in old.read_text().splitlines() if l.split('\t')[2]=='uncertified'}
 cs=[c for c in cs if c['study_index'] in ids or c['origin']!='count replay']
if '--start' in sys.argv:cs=[c for c in cs if c['study_index']>=int(sys.argv[sys.argv.index('--start')+1])]
if '--end' in sys.argv:cs=[c for c in cs if c['study_index']<int(sys.argv[sys.argv.index('--end')+1])]
dest=HERE/sys.argv[2]
with dest.open('w') as out:
 for c in cs:
  h=c['held'];line=' '.join(map(str,[c['study_index'],c['family'],len(c['data']),*c['data'],*[h.get(k,'NA') for k in KEYS],c.get('maxit',600 if c['family']=='bnbinom' else 500 if c['family']=='bbinom' else 200),c.get('tol',1e-10)]))+'\n'
  r=subprocess.run([sys.argv[1]],input=line,text=True,capture_output=True,check=True)
  out.write(r.stdout);out.flush()
  print(c['study_index'],c['family'],r.stdout.split('\t')[2],flush=True)
