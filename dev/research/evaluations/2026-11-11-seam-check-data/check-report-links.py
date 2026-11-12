"""Check edited Markdown and prove the unrelated link findings predate this job."""
import importlib.util,json,subprocess
from pathlib import Path
from urllib.parse import unquote,urlsplit
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
spec=importlib.util.spec_from_file_location('links',ROOT/'dev/tools/check_links.py')
module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module)
names=[p for p in subprocess.check_output(['git','diff','--name-only'],cwd=ROOT,text=True).splitlines() if p.endswith('.md')]
names+=['dev/research/evaluations/2026-11-11-seam-check-report.md']
errors=[];checked=0
for name in names:
    p=ROOT/name
    for _,url in module.destinations(p.read_text()):
        if url.startswith(('#','//')) or urlsplit(url).scheme:continue
        path=unquote(urlsplit(url).path)
        if not path:continue
        checked+=1
        target=ROOT/path.lstrip('/') if path.startswith('/') else p.parent/path
        if not target.exists():errors.append(dict(file=name,target=url))
global_lines=(ROOT/'build/scratch/seam-check/final-links.log').read_text().splitlines()[1:]
oldfiles=sorted({line.split(':',1)[0] for line in global_lines if line.strip()})
for name in oldfiles:
    assert name not in names
    assert subprocess.check_output(['git','show','HEAD:'+name],cwd=ROOT)==(ROOT/name).read_bytes(),name
record=dict(files=names,local_destinations_checked=checked,errors=errors,
            repository_wide_existing_findings=len(global_lines),unchanged_files_with_existing_findings=oldfiles)
(HERE/'link-check.json').write_text(json.dumps(record,indent=2)+'\n')
assert not errors,errors
print(f'{len(names)} edited/new Markdown files; {checked} local destinations; no broken links.')
print(f'{len(global_lines)} repository-wide findings are in byte-identical baseline files.')
