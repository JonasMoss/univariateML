"""Replay frozen reference inputs independently of an expanded retirement ledger."""
from pathlib import Path
from load_evidence import read_json
import json,gzip,subprocess
root=Path.cwd();dest=root/'dev/research/evaluations/2026-11-12-modifiers-2-data'
old=json.load(gzip.open(dest/'tbinom-default-inputs.json.gz','rt'))
subprocess.run(['Rscript',str(dest/'check_tbinom_defaults.R')],check=True)
new={r['case']:r for r in read_json(dest/'tbinom-default-after.json')}
differences=[]
for row in old['rows']:
 r={k:v for k,v in row.items() if k!='input'}
 current={k:new[r['case']][k] for k in r}
 if r!=current:differences.append(dict(before=r,after=current))
result=dict(baseline=old['baseline'],cases=len(old['rows']),differences=differences)
(dest/'tbinom-default-comparison.json').write_text(json.dumps(result,indent=2)+'\n')
print('Tbinom defaults:',len(old['rows']),'cases;',len(differences),'differences')
assert not differences
