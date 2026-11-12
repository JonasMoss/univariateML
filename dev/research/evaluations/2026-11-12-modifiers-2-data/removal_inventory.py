"""Read-only removal inventory, regenerated against the retained source tree."""
from pathlib import Path
import csv,json,re,yaml
root=Path.cwd()
ids=[r['family'] for r in csv.DictReader((root/'dev/validation/family-reductions.csv').open()) if r['retirement']=='pending']
paths=[]
for id in ids:
 for prefix in ['univariateML-cpp/include/cppunivariate/distributions','bindings/common/distributions','bindings/r/distributions']:
  paths.extend(str(p.relative_to(root)) for p in (root/prefix/id).rglob('*') if p.is_file())
references=[]
for prefix in ['bindings','site','univariateML-cpp','dev/validation']:
 for p in (root/prefix).rglob('*'):
  if not p.is_file() or any(x in p.parts for x in ['build','node_modules','generated','dist','.astro']):continue
  # site-check stages a generated source mirror; inventory the authoritative files.
  if p.is_relative_to(root/'site/public/sources'):continue
  if p.suffix not in ['.R','.Rmd','.md','.hpp','.cpp','.mjs','.ts','.astro','.py','.csv','.txt']:continue
  if str(p.relative_to(root)) in paths:continue
  try:lines=p.read_text().splitlines()
  except (UnicodeError,OSError):continue
  for i,line in enumerate(lines,1):
   found=[id for id in ids if re.search(r'(?<![a-z])(?:ml)?'+id+r'(?![a-z])',line)]
   if found:references.append(dict(path=str(p.relative_to(root)),line=i,families=found,text=line))
edges=[]
for p in (root/'univariateML-cpp/include/cppunivariate/distributions').glob('*/README.md'):
 m=yaml.safe_load(p.read_text().split('---',2)[1])
 for kind,values in m.get('relations',{}).items():
  for edge in values:
   if m['id'] in ids or edge.get('family') in ids:
    edges.append(dict(source=m['id'],kind=kind,**edge))
record=dict(families=ids,public_wrappers=['ml'+id for id in ids],files=sorted(paths),
 site_routes=[f'/distributions/{id}/{suffix}' for id in ids for suffix in ['', 'explore/']],
 relations=edges,news_lines=[r for r in references if r['path']=='bindings/r/NEWS.md'],references=references)
(root/'dev/research/evaluations/2026-11-12-modifiers-2-data/removal.json').write_text(json.dumps(record,indent=2)+'\n')
print(len(paths),'family files;',len(references),'external reference lines')
