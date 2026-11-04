import ast,hashlib,json,re,subprocess
from pathlib import Path
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
REPORT=HERE.parent/'2026-11-04-t-cert-report.md'
files=[p for p in HERE.iterdir() if p.is_file() and p.name!='final-checks.json']+[REPORT]
for fam in ['lst','std']:files.append(ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{fam}/README.md')
links=0
for p in files:
    if p.suffix=='.py':ast.parse(p.read_text(),filename=str(p))
    if p.suffix=='.json':json.loads(p.read_text())
    if p.suffix=='.md':
        for target in re.findall(r'\]\(([^)]+)\)',p.read_text()):
            if '://' in target or target.startswith('#'):continue
            path=target.split('#')[0]
            destination=(p.parent/path).resolve()
            assert destination.exists() or destination==HERE/'final-checks.json',(p,target)
            links+=1
    assert not any(line.rstrip()!=line for line in p.read_text().splitlines()),p
validation=json.loads((HERE/'validation.json').read_text())
for name,expected in validation['hashes'].items():assert hashlib.sha256((ROOT/name).read_bytes()).hexdigest()==expected,name
subprocess.run(['git','diff','--check'],cwd=ROOT,check=True)
changed=subprocess.check_output(['git','diff','--name-only'],cwd=ROOT,text=True).splitlines()
assert set(changed)=={f'univariateML-cpp/include/cppunivariate/distributions/{fam}/README.md' for fam in ['lst','std']},changed
out={'files_checked':len(files),'local_links_checked':links,'recorded_hashes_checked':len(validation['hashes']),
     'tracked_changes':changed,'report_sha256':hashlib.sha256(REPORT.read_bytes()).hexdigest(),
     'result':'passed'}
(HERE/'final-checks.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
