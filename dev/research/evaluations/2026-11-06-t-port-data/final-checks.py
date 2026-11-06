"""Final document/source checks after all requested regression tiers passed."""
import ast,hashlib,json,os,subprocess,sys
from pathlib import Path
here=Path(__file__).resolve().parent
root=here.parents[3]
os.environ['GIT_OPTIONAL_LOCKS']='0'
result={'status':'running'}
output=here/'final-checks.json'
output.write_text(json.dumps(result)+'\n')
validation=json.loads((here/'validation.json').read_text())
for name,expected in validation['source_sha256'].items():
    assert hashlib.sha256((root/name).read_bytes()).hexdigest()==expected,('source changed after final validation',name)
for p in here.glob('*.py'):ast.parse(p.read_text(),filename=str(p))
for p in here.rglob('*.json'):json.loads(p.read_text(),parse_constant=lambda s:(_ for _ in ()).throw(ValueError(s)))
whitespace=subprocess.run(['git','diff','--check'],cwd=root,capture_output=True,text=True)
assert whitespace.returncode==0,whitespace.stdout+whitespace.stderr
links=subprocess.run([sys.executable,str(root/'dev/tools/check_links.py')],cwd=root,capture_output=True,text=True)
expected={
    'dev/research/evaluations/2026-11-03-boundary-lit-report.md:5: ../boundary-points.pdf',
    'univariateML-cpp/notes/inference.md:3: /inference/',
    'univariateML-cpp/notes/inference.md:95: /inference/',
}
findings={line for line in links.stdout.splitlines()[1:] if line.strip()}
assert links.returncode==1 and findings==expected,links.stdout+links.stderr
for name in {line.split(':',1)[0] for line in findings}:
    original=subprocess.check_output(['git','show','164d41784756606a48ddc0cedc67752acab4050b:'+name],cwd=root)
    assert (root/name).read_bytes()==original,('finding not baseline',name)
for family in ['lst','std']:
    name=f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json'
    original=subprocess.check_output(['git','show','164d41784756606a48ddc0cedc67752acab4050b:'+name],cwd=root)
    assert (root/name).read_bytes()==original,name
report=here.parent/'2026-11-06-t-port-report.md'
result=dict(status='passed',source_identities_unchanged=True,python_syntax=True,json=True,whitespace=True,changed_document_links=True,repository_link_scan={'returncode':links.returncode,'summary':links.stdout.splitlines()[0],'baseline_findings':sorted(findings),'baseline_sources_verified_unchanged':True},goldens_unchanged=True,report_sha256=hashlib.sha256(report.read_bytes()).hexdigest(),evidence_sha256={str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(here.rglob('*')) if p.is_file() and p!=output})
output.write_text(json.dumps(result,indent=2)+'\n')
print(links.stdout.strip())
print('All new/changed document links pass; the three unchanged baseline findings are recorded separately.')
print('Final report, source identity, JSON, syntax, golden and whitespace checks passed.')
