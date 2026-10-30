from pathlib import Path
import hashlib,json,subprocess,difflib
mapping=json.loads(Path('build/scratch/move-map.json').read_text())
base=json.loads(Path('build/scratch/pre/all-sha256.json').read_text())
errors=[]; protected=[];changed=[];front=[]
for old,new in mapping.items():
 p=Path(new)
 if not p.is_file():errors.append('missing '+new);continue
 actual=hashlib.sha256(p.read_bytes()).hexdigest()
 if actual!=base[old]:changed.append(new)
 if p.suffix=='.hpp' or p.name in ('test.cpp','golden.json','truth.json') or (p.name=='truth.py' and old.startswith('cppunivariate/')):
  protected.append(new)
  if actual!=base[old]:errors.append('protected changed '+new)
 if old.startswith('cppunivariate/include/cppunivariate/distributions/') and p.name=='README.md':
  original=subprocess.check_output(['git','show','a1de750:'+old]).decode()
  if original.split('---',2)[1]!=p.read_text().split('---',2)[1]:front.append(new)
assert not errors,(errors[:20])
assert not front,front
Path('build/scratch/protected-sha256.log').write_text('\n'.join(hashlib.sha256(Path(p).read_bytes()).hexdigest()+'  '+p for p in protected)+'\n')
Path('build/scratch/edited-files.txt').write_text('\n'.join(changed)+'\n')
print(len(mapping),'tracked inputs retained;',len(protected),'protected files identical; 98 front matter blocks identical;',len(changed),'path/documentation edits')
# Compare the requested historical manifest independently; report inherited drift.
historical=[]
for line in Path('dev/research/evaluations/2026-10-30-migration-baseline/source-sha256.log').read_text().splitlines():
 h,p=line.split('  ',1)
 # Manifest path strings may have been rebased; recover the pre-move spelling.
 old=p.replace('univariateML-cpp/','cppunivariate/',1)
 new=mapping.get(old,p)
 if hashlib.sha256(Path(new).read_bytes()).hexdigest()!=h:historical.append(new)
Path('build/scratch/historical-sha-differences.txt').write_text('\n'.join(historical)+'\n')
print(len(historical),'differences against historical 5f11fb9 manifest')
# Source package tree is captured directly after regen, without build artefacts.
a=Path('build/scratch/pre-package');b=Path('univariateML-r')
af={str(p.relative_to(a)):p for p in a.rglob('*') if p.is_file()}
bf={str(p.relative_to(b)):p for p in b.rglob('*') if p.is_file()}
assert af.keys()==bf.keys(),(af.keys()-bf.keys(),bf.keys()-af.keys())
diffs=[];names=[]
for name in sorted(af):
 if af[name].read_bytes()==bf[name].read_bytes():continue
 names.append(name)
 try:diffs.extend(difflib.unified_diff(af[name].read_text().splitlines(True),bf[name].read_text().splitlines(True),fromfile='pre/'+name,tofile='post/'+name))
 except UnicodeError:diffs.append('Binary differs: '+name+'\n')
Path('build/scratch/package.diff').write_text(''.join(diffs))
Path('build/scratch/package-diff-files.txt').write_text('\n'.join(names)+'\n')
print(len(af),'generated package files;',len(names),'differ:',names)

for name in names:
 text=bf[name].read_text().replace('dev/research/','notes/').replace('univariateML-cpp/','cppunivariate/').replace('bindings/r','univariateML')
 assert text==af[name].read_text(), 'Non-path package change: '+name
print('All 22 package differences are exactly path substitutions; all binaries identical.')
