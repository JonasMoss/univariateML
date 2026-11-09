#!/usr/bin/env python3
"""Validate completed audit evidence, source identities and report links."""
import ast
import gzip
import hashlib
import json
import re
import subprocess
from pathlib import Path

here=Path(__file__).resolve().parent
root=here.parents[3]
report=here.parent/'2026-11-09-proof-audit-report.md'
digest=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
main=json.loads((here/'results.json').read_text())
boundary=json.loads((here/'boundaries.json').read_text())
provenance=json.loads((here/'provenance.json').read_text())
assert main['result']==boundary['result']=='PASS'
assert main['decimal_digits']==boundary['digits']==120
assert main['script_sha256']==digest(here/'check.py')
assert boundary['script_sha256']==digest(here/'boundaries.py')
assert boundary['parent_script_sha256']==digest(here/'check.py')
for filename,expected in provenance['numerical_results'].items():
    assert digest(here/filename)==expected,filename
for filename,expected in provenance['sources'].items():
    assert digest(root/filename)==expected,filename
for filename,expected in provenance['audit_scripts'].items():
    assert digest(here/filename)==expected,filename
for filename,metadata in provenance['logs'].items():
    raw=gzip.decompress((here/filename).read_bytes())
    assert hashlib.sha256(raw).hexdigest()==metadata['uncompressed_sha256'],filename
    assert len(raw)==metadata['bytes'],filename
for p in here.glob('*.py'):
    ast.parse(p.read_text(),filename=str(p))
for p in here.glob('*.sh'):
    subprocess.run(['bash','-n',str(p)],check=True)
rows=[line.split('\t') for line in (here/'suites.tsv').read_text().splitlines()]
required=[('workstation','cpp-dev'),('workstation','cpp-full'),('workstation','cpp-double'),('workstation','r-dev'),('simbox','r-dev'),('simbox','r-full'),('simbox','cpp-double')]
completed=[]
for machine,mode in required:
    matches=[r for r in rows if r[0]==machine and r[1]==mode]
    assert matches,(machine,mode)
    latest=matches[-1]
    used_machine=machine
    if latest[4]=='75':
        fallback=[r for r in rows if r[0]=='workstation-fallback' and r[1]==mode and r[4]=='0']
        assert fallback,(machine,mode,'missing successful fallback')
        used_machine='workstation-fallback'
    else:
        assert latest[4]=='0',latest
    text=(root/f'build/scratch/proof-audit/{used_machine}-{mode}.log').read_text()
    if mode.startswith('cpp'):
        match=re.findall(r'100% tests passed(?:, 0 tests failed)? out of (\d+)',text)
        assert match,(machine,mode,'missing green ctest summary')
        result={'shards':int(match[-1])}
    else:
        match=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',text)
        assert match,(machine,mode,'missing R summary')
        fail,warn,skip,passed=map(int,match[-1])
        assert fail==warn==0
        assert mode!='r-full' or skip==0
        result={'pass':passed,'skip':skip,'fail':fail,'warn':warn}
    completed.append({'machine':used_machine,'suite':mode,**result})
for name in ['results.json','boundaries.json','provenance.json','suites.tsv']:
    assert (here/name).is_file()
body=report.read_text()
assert body.endswith('\n')
assert all(line==line.rstrip() for line in body.splitlines()),'trailing whitespace'
links=[]
for target in re.findall(r'\]\(([^)]+)\)',body):
    if target.startswith(('https://','http://','#')):
        continue
    path=target.split('#',1)[0]
    if not path:
        continue
    linked=(report.parent/path).resolve()
    assert linked.exists(),target
    links.append(target)
subprocess.run(['git','--no-optional-locks','diff','--exit-code'],cwd=root,check=True,capture_output=True)
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()==provenance['environment']['revision']
untracked=subprocess.check_output(['git','ls-files','--others','--exclude-standard'],cwd=root,text=True).splitlines()
for filename in untracked:
    assert filename=='dev/research/evaluations/2026-11-09-proof-audit-report.md' or filename.startswith('dev/research/evaluations/2026-11-09-proof-audit-data/'),filename
out={'result':'PASS','report_sha256':digest(report),'source_revision':provenance['environment']['revision'],'recorded_main_comparisons':main['recorded_comparisons'],'cover_cells':main['cover_cells'],'recorded_boundary_comparisons':boundary['assertions'],'root_brackets':len(boundary['root_brackets']),'suites':completed,'local_links_checked':len(links),'production_diff':'empty','audit_script_count':len(provenance['audit_scripts'])}
(here/'validation.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
