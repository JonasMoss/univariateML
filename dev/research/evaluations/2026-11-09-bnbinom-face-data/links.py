"""Audit new Markdown and retain, rather than suppress, baseline findings."""
import importlib.util
import json
import subprocess
import sys
from pathlib import Path
from urllib.parse import unquote,urlsplit
here=Path(__file__).parent
result=subprocess.run([sys.executable,'dev/tools/check_links.py'],capture_output=True,text=True)
(here/'global-links.txt').write_text(result.stdout+result.stderr)
reported=[]
for line in result.stdout.splitlines()[1:]:
    if not line:continue
    name=line.split(':',1)[0];p=Path(name)
    assert p.read_bytes()==subprocess.check_output(['git','show','b682b747713d6073a56f162b5135657c00806064:'+name]),name
    reported.append(line)
spec=importlib.util.spec_from_file_location('links','dev/tools/check_links.py')
module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module)
files=[here.parent/'2026-11-09-bnbinom-face-report.md',here/'derivations.md',Path('univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md')]
checked=0;errors=[]
for file in files:
    text=file.read_text()
    for offset,url in module.destinations(text):
        if url.startswith(('#','//')) or urlsplit(url).scheme:continue
        path=unquote(urlsplit(url).path)
        if not path:continue
        checked+=1
        target=module.ROOT/path.lstrip('/') if path.startswith('/') else file.parent/path
        if not target.exists():errors.append(f'{file}:{text[:offset].count(chr(10))+1}: {url}')
out=dict(global_exit=result.returncode,global_findings=len(reported),all_reported_files_identical_to_base=True,
         changed_markdown_files=[str(f) for f in files],changed_local_destinations=checked,changed_errors=errors)
(here/'links.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2));assert not errors,errors
