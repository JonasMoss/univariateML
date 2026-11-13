#!/usr/bin/env python3
"""Inventory literal grep hits, including substring coincidences, without histories."""
from pathlib import Path
import csv,json,subprocess,collections
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
ids=list(json.loads((ROOT/'bindings/common/modifier-laws.json').read_text()))
pattern='|'.join(ids+['ml'+i for i in ids])
command=['rg','--json','--hidden','-g','!.git/**','-g','!dev/research/**','-g','!dev/roadmap/**',pattern,'.']
proc=subprocess.run(command,cwd=ROOT,text=True,capture_output=True)
if proc.returncode not in [0,1]:raise RuntimeError(proc.stderr)
moved=set(json.loads((HERE/'moves.json').read_text()).values())
rows=[]
for line in proc.stdout.splitlines():
 item=json.loads(line)
 if item['type']!='match':continue
 d=item['data'];path=d['path']['text'].removeprefix('./');text=d['lines']['text'].rstrip('\n')
 matched=[i for i in ids if i in text]
 exact=[i for i in matched if __import__('re').search(r'(?<![a-z])(?:ml)?'+i+r'(?![a-z])',text)]
 if matched==['zip'] and __import__('re').search(r'\b(?:gzip|zip)\s*\(',text):reason='Python zip/gzip operation; no retired family reference'
 elif not exact:reason='Substring coincidence (for example logitnorm, gzip or Python zip); no retired family reference'
 elif path in moved:reason='Retained modifier kernel, binding adapter or frozen fixture at its new path'
 elif path in ['dev/STATUS.md','dev/KNOWN_ISSUES.md','AGENTS.md']:reason='Protected project/status instructions retained under the job scope'
 elif path=='bindings/r/NEWS.md':reason='Migration entry or earlier release history'
 elif path.startswith('univariateML-cpp/include/') and '/core/' not in path:reason='Retained internal modifier kernel, helper or kernel include'
 elif path.endswith(('core/discrete_stop.hpp','common/fit.hpp')):reason='Frozen internal stop-model payload identifier; retained for exact snapshot invariance'
 elif path.startswith('univariateML-cpp/include/cppunivariate/core/'):
  reason='Shared modifier machinery or frozen internal stop coordinates; no retired public family registration'
 elif path.startswith('bindings/r/distributions/') and 'expect_identical(e$model' in text:reason='Modifier regression asserts a frozen internal stop-model code, not a public wrapper'
 elif '/tests/' in path or '/modifier-tests/' in path or path.startswith('bindings/js/') and 'test' in path:reason='Retained kernel regression or test-only map to a public modifier route'
 elif path.startswith('bindings/common/modifier') or path.startswith('bindings/r/modifier-') or path in ['bindings/r/R/modifiers.R','bindings/r/R/modifier_law_metadata.R','bindings/r/scripts/modifier_laws.R','bindings/js/modifiers.mjs']:reason='Private modifier binding/evaluation coordinates; absent from the public family registry'
 elif path.startswith('dev/validation/modifier-fixtures/'):reason='Frozen modifier golden/truth evidence under its provenance label'
 elif path=='dev/validation/test-tier-inventory.csv':reason='Frozen measured test-tier baseline retains original test labels for attribution'
 elif path.startswith('dev/validation/') or path in ['dev/tools/check_links.py']:reason='Validation/historical provenance or internal kernel coordinate label, not a public family registration'
 elif path=='bindings/js/modifier-metadata.mjs':reason='Generated modifier capabilities include private kernel provenance; all public registry IDs survive'
 elif path=='bindings/js/measure-all.mjs':reason='gzip size measurement; no retired family reference'
 elif path.startswith('bindings/r/scripts/') or path=='bindings/js/generate.py':reason='Generator handling private modifier evaluation kernels'
 elif path.endswith('.md'):reason='Mathematical kernel terminology or linked migration/design context'
 else:reason='REVIEW'
 rows.append(dict(path=path,line=d['line_number'],ids=';'.join(matched),reason=reason,text=text))
with (HERE/'remaining-references.csv').open('w') as f:
 writer=csv.DictWriter(f,fieldnames=['path','line','ids','reason','text'],lineterminator='\n');writer.writeheader();writer.writerows(rows)
summary=collections.Counter(r['reason'] for r in rows)
(HERE/'remaining-reference-summary.json').write_text(json.dumps(dict(summary),indent=2)+'\n')
print(json.dumps(dict(summary),indent=2))
for row in rows:
 if row['reason']=='REVIEW':print(row)

if any(row["reason"]=="REVIEW" for row in rows):raise SystemExit(1)
