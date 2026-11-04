"""Read-only checks of final source scope, evidence syntax and document links."""
import ast,hashlib,json,os,re,subprocess
from pathlib import Path
os.environ['GIT_OPTIONAL_LOCKS']='0'
out=Path(__file__).resolve().parent
report=out.parent/'2026-11-04-burr-deps-report.md'
allowed={'bindings/common/distributions/invburr/fit.hpp','bindings/r/distributions/invburr/test.R'}
for family in ['invburr','pareto4']:
    allowed.update(f'univariateML-cpp/include/cppunivariate/distributions/{family}/{name}' for name in ['mle.hpp','README.md','test.cpp'])
names=set(subprocess.check_output(['git','diff','--name-only'],text=True).splitlines())
assert names<=allowed,names-allowed
subprocess.run(['git','diff','--check'],check=True)
for p in out.glob('*.py'):ast.parse(p.read_text())
for name in ['run.sh','verify.sh']:subprocess.run(['sh','-n',str(out/name)],check=True)
for p in out.glob('*.json'):json.loads(p.read_text())
documents=[report,out/'README.md']+[Path(f'univariateML-cpp/include/cppunivariate/distributions/{family}/README.md') for family in ['invburr','pareto4']]
links=[]
for p in documents:
    text=p.read_text()
    assert all(line==line.rstrip() for line in text.splitlines()),p
    for target in re.findall(r'\[[^\]]*\]\(([^)]+)\)',text):
        if target.startswith(('https:','http:','mailto:')):continue
        file=target.split('#')[0]
        if not file:continue
        path=(p.parent/file).resolve()
        assert path.exists(),(p,target)
        links.append({'document':str(p),'target':target})
result={'tracked_source_scope':'passed','diff_whitespace':'passed','python_shell_json_syntax':'passed','local_links_checked':len(links),'report_sha256':hashlib.sha256(report.read_bytes()).hexdigest(),'documents':[str(p) for p in documents]}
(out/'final-checks.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
