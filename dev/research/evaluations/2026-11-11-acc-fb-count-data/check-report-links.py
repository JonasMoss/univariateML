"""Check local links in this report and the five changed family dossiers."""
import importlib.util,json,pathlib,sys
from urllib.parse import unquote,urlsplit
sys.dont_write_bytecode=True
P=pathlib.Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('umvl_links','dev/tools/check_links.py')
module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module)
files=[P.parent/'2026-11-11-acc-fb-count-report.md']
files.extend(pathlib.Path('univariateML-cpp/include/cppunivariate/distributions',family,'README.md') for family in ['bbinom','tbinom','ztbinom','zib','zmbinom'])
record=P/'markdown-link-check.json'
record.write_text('{}\n') # The report links to this receipt itself.
checked=0;errors=[]
for file in files:
 for _,url in module.destinations(file.read_text()):
  if url.startswith(('#','//')) or urlsplit(url).scheme:continue
  name=unquote(urlsplit(url).path)
  if not name:continue
  target=module.ROOT/name.lstrip('/') if name.startswith('/') else file.parent/name
  checked+=1
  if not target.exists():errors.append(dict(source=str(file),target=url))
out=dict(files=[str(f) for f in files],local_destinations=checked,errors=errors)
record.write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
assert not errors
