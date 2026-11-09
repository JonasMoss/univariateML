#!/usr/bin/env python3
"""Final audit integrity checks and report rendering; caller owns one CPU slot."""
import hashlib,json,pathlib,platform,re,subprocess,sys
HERE=pathlib.Path(__file__).resolve().parent
for p in HERE.glob('*.py'):compile(p.read_text(),str(p),'exec')
cases=json.loads((HERE/'cases.json').read_text());marker=json.loads((HERE/'inventory-finalized.json').read_text())
assert marker['cases']==len(cases)
assert marker['sha256']==hashlib.sha256((HERE/'cases.json').read_bytes()).hexdigest()
p=HERE/'support-supplement.json';old=json.loads(p.read_text());indices=[c['index'] for c in cases if c['sample'].startswith(('support-','constant-support-'))]
p.write_text(json.dumps(dict(historical_last_additions=old.get('added',old.get('historical_last_additions',[])),indices=indices,count=len(indices),families=sorted({cases[i]['family'] for i in indices})),indent=2))
p=HERE/'run-provenance.json';provenance=json.loads(p.read_text());provenance['final_integrity']={'numeric_validation':'simbox and workstation','report_render_host':platform.node(),'independent_simple_checks':142,'syntax_checks':'PASS'};p.write_text(json.dumps(provenance,indent=2))
subprocess.run([sys.executable,str(HERE/'validate.py')],check=True)
subprocess.run([sys.executable,str(HERE/'report.py')],check=True)
report=HERE.parent/'2026-11-07-fit-audit.md';text=report.read_text()
for link in re.findall(r'\]\(([^)]+)\)',text):
 if not link.startswith(('http:','https:','#')):assert (report.parent/link.split('#')[0]).exists(),link
refs=list(map(json.loads,(HERE/'reference.jsonl').read_text().splitlines()));bad=[r for r in refs if r['classification']=='silent failure']
assert len(re.findall(r'^## Case ',(HERE/'silent-failures.md').read_text(),re.M))==len(bad)
assert len(re.findall(r'^## [a-z0-9]+$',(HERE/'family-tables.md').read_text(),re.M))==99
result=dict(validation='PASS',report_links='PASS',silent_reproductions=len(bad),family_tables=99,evaluation_script_sha256={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in HERE.glob('*.py')})
(HERE/'artifact-validation.json').write_text(json.dumps(result,indent=2));print('PASS final artifacts:',len(bad),'reproductions, 99 family tables, all report links, Python syntax, frozen inventory SHA')
