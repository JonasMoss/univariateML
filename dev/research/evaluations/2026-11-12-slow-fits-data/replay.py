"""Run under run; compile replay.cpp first against the requested source tree.
Baseline: c4ed89f6; recover headers with git archive c4ed89f6 univariateML-cpp/include.
The guard is evidence collection only, never a fitter outcome.
"""
import gzip,json,subprocess,sys
from pathlib import Path
root=Path(__file__).resolve().parents[4]
exe,output=sys.argv[1:]
cases=json.load(gzip.open(root/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz','rt'))
with open(output,'w') as out:
 for c in cases:
  if c['family'] not in ('cmb','dgamma'):continue
  h=c['held'];line=' '.join(map(str,[c['family'],len(c['data']),*c['data'],*[h.get(k,'nan') for k in ('size','prob','nu','shape','rate')]]))+'\n'
  try:
   p=subprocess.run([exe],input=line,text=True,capture_output=True,check=True,timeout=120)
   result=p.stdout.strip()
  except subprocess.TimeoutExpired:result='timeout 120'
  row=dict(index=c['index'],family=c['family'],result=result);out.write(json.dumps(row)+'\n');out.flush();print(row,flush=True)
