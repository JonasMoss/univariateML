import gzip,hashlib,importlib.util,json,os,re,subprocess
from pathlib import Path
from urllib.parse import unquote,urlsplit
root=Path.cwd();p=root/'dev/research/evaluations/2026-11-10-fallback-count-data'
spec=importlib.util.spec_from_file_location('links',root/'dev/tools/check_links.py');m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
log=gzip.decompress((p/'logs/markdown-links.log.gz').read_bytes()).decode(); lines=log.splitlines()[1:]; prior=sorted(set(line.split(':',1)[0] for line in lines))
unchanged=[]
for f in prior:
 old=subprocess.check_output(['git','show','HEAD:'+f],env=os.environ|{'GIT_OPTIONAL_LOCKS':'0'}); assert old==(root/f).read_bytes(),f;unchanged.append(f)
files=[root/'dev/research/evaluations/2026-11-10-fallback-count-report.md']+[root/f for f in json.loads((p/'provenance.json').read_text())['source_sha256'] if f.endswith('.md')]
errors=[];count=0
for file in files:
 for offset,url in m.destinations(file.read_text()):
  if url.startswith(('#','//')) or urlsplit(url).scheme:continue
  path=unquote(urlsplit(url).path)
  if not path:continue
  count+=1;target=root/path.lstrip('/') if path.startswith('/') else file.parent/path
  if not target.exists():errors.append(str(file)+': '+url)
assert not errors,errors
result={'repository_wide_checker_exit':1,'repository_wide_broken':len(lines),'all_reported_problem_files_byte_identical_to_HEAD':unchanged,'owned_markdown_files':len(files),'owned_destinations':count,'owned_broken':errors}
(p/'markdown-links.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result,indent=2))
