"""Use the repository checker parser; separate baseline debt from scoped links."""
import importlib.util,json,subprocess
from pathlib import Path
from urllib.parse import unquote,urlsplit
D=Path(__file__).resolve().parent
ROOT=D.parents[3]
spec=importlib.util.spec_from_file_location('check_links',ROOT/'dev/tools/check_links.py')
m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
base='bc8ff7be2e3bc4b9186a56fc4f2e8ee45c163714'
def check(files):
    errors=[];checked=0
    for f in files:
        text=f.read_text()
        for offset,url in m.destinations(text):
            if url.startswith(('#','//')) or urlsplit(url).scheme: continue
            path=unquote(urlsplit(url).path)
            if not path: continue
            checked+=1
            target=ROOT/path.lstrip('/') if path.startswith('/') else f.parent/path
            if not target.exists(): errors.append({'file':str(f.relative_to(ROOT)),'line':text[:offset].count('\n')+1,'url':url})
    return {'files':len(files),'destinations':checked,'errors':errors}
files=sorted(ROOT.glob('*.md'))
for name in ('dev','bindings','univariateML-cpp'):
    for ext in ('*.md','*.Rmd'): files.extend(sorted((ROOT/name).rglob(ext)))
files=[f for f in files if 'build' not in f.relative_to(ROOT).parts and '__pycache__' not in f.parts]
global_result=check(files)
unchanged={}
for row in global_result['errors']:
    p=row['file'];blob=subprocess.check_output(['git','--no-optional-locks','show',base+':'+p],cwd=ROOT)
    unchanged[p]=blob==(ROOT/p).read_bytes()
assert all(unchanged.values())
scoped=check([D.parent/'2026-11-11-acc-fb-t-report.md']+[ROOT/'univariateML-cpp/include/cppunivariate/distributions'/f/'README.md' for f in ('std','lst','sstd','sged','st','slash')])
assert not scoped['errors']
result={'baseline':base,'global':global_result,'broken_source_byte_identical_to_baseline':unchanged,'scoped':scoped}
(D/'baseline-link-audit.json').write_text(json.dumps(result,indent=2)+'\n')
print(f"Scoped: {scoped['files']} files, {scoped['destinations']} destinations, 0 broken. Global: {len(global_result['errors'])} inherited diagnostics; {len(unchanged)} source files byte-identical to baseline.")
