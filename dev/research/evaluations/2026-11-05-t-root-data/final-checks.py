"""Document/source consistency after the final report is written."""
import ast
import hashlib
import json
import re
import subprocess
from pathlib import Path

HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
report=HERE.parent/'2026-11-05-t-root-report.md'
assert report.is_file()
for path in HERE.glob('*.py'):ast.parse(path.read_text(),filename=str(path))
for path in HERE.glob('*.json'):json.loads(path.read_text())
for path in HERE.glob('*.sh'):subprocess.run(['sh','-n',str(path)],check=True)
docs=[report,HERE/'README.md',HERE/'derivation.md']
docs.extend(ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{family}/README.md' for family in ['lst','std'])
links=[]
for path in docs:
    for target in re.findall(r'\[[^\]]*\]\(([^)]+)\)',path.read_text()):
        if target.startswith(('https:','http:','#','/')):continue
        destination=(path.parent/target.split('#')[0]).resolve()
        assert destination.exists(),(path,target)
        links.append({'source':str(path.relative_to(ROOT)),'target':target})
validation=json.loads((HERE/'validation.json').read_text())
for name,digest in validation['hashes'].items():
    assert hashlib.sha256((ROOT/name).read_bytes()).hexdigest()==digest,name
diff=subprocess.check_output(['git','diff','--name-only'],cwd=ROOT,text=True).splitlines()
allowed={f'univariateML-cpp/include/cppunivariate/distributions/{f}/README.md' for f in ['lst','std']}
assert set(diff)<=allowed,diff
subprocess.run(['git','diff','--check'],cwd=ROOT,check=True)
result={'local_links_checked':len(links),'syntax':'passed','JSON':'passed','recorded_hashes':'passed',
        'preexisting_changed_files':diff,'whitespace':'passed','report_sha256':hashlib.sha256(report.read_bytes()).hexdigest()}
(HERE/'final-checks.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
