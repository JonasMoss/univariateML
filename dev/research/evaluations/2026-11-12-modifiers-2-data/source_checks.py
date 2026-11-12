"""Check authored links and distinguish existing repository link failures."""
from pathlib import Path
from urllib.parse import unquote, urlsplit
import hashlib, importlib.util, json, subprocess
root=Path.cwd(); dest=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('check_links',root/'dev/tools/check_links.py')
module=importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
changes=subprocess.check_output(['git','diff','--name-only'],text=True).splitlines()
report='dev/research/evaluations/2026-11-12-modifiers-2-report.md'
authored=[p for p in changes if p.endswith(('.md','.Rmd'))]
if (root/report).exists():authored.append(report)
checked=0;errors=[]
for name in authored:
    file=root/name;text=file.read_text()
    for offset,url in module.destinations(text):
        if url.startswith(('#','//')) or urlsplit(url).scheme:continue
        path=unquote(urlsplit(url).path)
        if not path:continue
        checked+=1;target=root/path.lstrip('/') if path.startswith('/') else file.parent/path
        if not target.exists():errors.append(dict(path=name,line=text[:offset].count('\n')+1,destination=url))
baseline=[]
whole=subprocess.run(['python3','dev/tools/check_links.py'],capture_output=True,text=True)
log=whole.stdout
assert whole.returncode==1,(whole.returncode,whole.stderr)
for line in log.splitlines():
    fields=line.split(':',2)
    if len(fields)!=3 or not fields[1].isdigit():continue
    name=fields[0];current=(root/name).read_bytes()
    original=subprocess.check_output(['git','show','fe17e3b8:'+name])
    assert current==original,name
    baseline.append(dict(path=name,line=int(fields[1]),destination=fields[2].strip(),sha256=hashlib.sha256(current).hexdigest()))
assert len(baseline)==45,len(baseline)
record=dict(baseline='fe17e3b8',authored_files=authored,authored_destinations=checked,new_broken=errors,existing_broken=baseline)
(dest/'source-checks.json').write_text(json.dumps(record,indent=2)+'\n')
print(len(authored),'authored Markdown files;',checked,'destinations;',len(errors),'new broken links; 45 baseline findings verified unchanged')
assert not errors,errors
