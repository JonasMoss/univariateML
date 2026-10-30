from pathlib import Path
import json,hashlib,difflib,subprocess,re,shutil
out=Path('dev/research/evaluations/2026-10-30-layout-evidence');out.mkdir(exist_ok=True)
base='a1de750e2367bafb98ac771b1fcf5a5f13fcd471'
mapping=json.loads(Path('build/scratch/move-map.json').read_text())
# Full source edit inventory, explicitly grouped by destination file.
patch=[];edits=['file\told_path\tchange']
for old,new in mapping.items():
 before=subprocess.check_output(['git','show',base+':'+old])
 after=Path(new).read_bytes()
 if before==after:continue
 edits.append(new+'\t'+old+'\tpath/link update'+('; layout documentation' if new in ('AGENTS.md','CLAUDE.md') or new.endswith('README_SCHEMA.md') else ''))
 try:patch.extend(difflib.unified_diff(before.decode().splitlines(True),after.decode().splitlines(True),fromfile=old,tofile=new))
 except UnicodeError:raise AssertionError('Changed binary '+new)
(out/'source-edits.tsv').write_text('\n'.join(edits)+'\n')
(out/'source-edits.patch').write_text(''.join(patch))
(out/'move-map.tsv').write_text('old\tnew\n'+'\n'.join(a+'\t'+b for a,b in mapping.items() if a!=b)+'\n')
for name in ('protected-sha256.log','historical-sha-differences.txt','package.diff','package-diff-files.txt'):
 shutil.copyfile('build/scratch/'+name,out/name)
# Keep requested hash inventory for the actual base as independent provenance.
manifest=json.loads(Path('build/scratch/pre/all-sha256.json').read_text())
(out/'base-sha256.log').write_text('\n'.join(h+'  '+mapping[p] for p,h in manifest.items())+'\n')
# Exact ctest case/assertion totals must match for all six targets.
def totals(path):
 s=path.read_text();return re.findall(r'\[doctest\] (?:test cases|assertions):\s+(\d+)\s*\|\s+(\d+) passed\s*\|\s+(\d+) failed(?:\s*\|\s+(\d+) skipped)?',s)
assert totals(Path('build/scratch/pre/cpp-targets.log'))==totals(Path('build/scratch/post/cpp-targets.log'))
for stage in ('pre','post'):
 for name in ('cpp-targets.log','ccache-stats.log','ccache-stats.tsv','exits.log'):
  shutil.copyfile(f'build/scratch/{stage}/{name}',out/f'{stage}-{name}')
 s=Path(f'build/scratch/{stage}/test.full.log').read_text()
 summary=re.findall(r'\[ FAIL \d+ \| WARN \d+ \| SKIP \d+ \| PASS \d+ \]',s)
 (out/f'{stage}-test-summary.log').write_text('\n'.join(summary)+'\n')
assert (out/'pre-test-summary.log').read_text()==(out/'post-test-summary.log').read_text()
# Compare each family and each subcheck status (not timing or oracle trajectory).
rows=['family\tpre_logLik\tpost_logLik\tpre_derivatives\tpost_derivatives\tpre_oracle\tpost_oracle\tpre_pass\tpost_pass']
for d in sorted(Path('univariateML-cpp/include/cppunivariate/distributions').glob('*/README.md')):
 id=d.parent.name
 a=json.loads(Path(f'build/scratch/pre/{id}.json').read_text());b=json.loads(Path(f'build/scratch/post/{id}.json').read_text())
 values=[]
 for key in ('logLik','score_hessian','oracle'):
  values.extend([a[key]['status'],b[key]['status']]);assert a[key]['status']==b[key]['status'],(id,key)
 assert a['passed']==b['passed']==True,id
 rows.append('\t'.join([id,*values,str(a['passed']),str(b['passed'])]))
(out/'quality-comparison.tsv').write_text('\n'.join(rows)+'\n')
# Cache deltas for the unchanged rebuild, without resetting shared counters.
def stats(p):return {k:int(v) for k,v in (l.split() for l in p.read_text().splitlines())}
times=['phase\trecipe\twall_seconds\tcache_hits\tcache_misses\thit_rate']
for phase in ('pre','post','rebuild'):
 for target in (('cpp-test','test','quality','regen') if phase!='rebuild' else ('cpp-test','regen')):
  p=Path(f'build/scratch/{phase}/{target}.time');secs=p.read_text().strip().split('=')[1]
  hits=miss=rate='not recorded per step'
  b=Path(f'build/scratch/{phase}/{target}.cache-before.tsv');a=Path(f'build/scratch/{phase}/{target}.cache-after.tsv')
  if a.exists() and b.exists():
   before=stats(b);after=stats(a)
   hits=sum(after[k]-before[k] for k in ('direct_cache_hit','preprocessed_cache_hit'));miss=after['cache_miss']-before['cache_miss']
   rate=f'{100*hits/(hits+miss):.2f}%' if hits+miss else 'N/A (no compiler calls)'
  times.append('\t'.join(map(str,[phase,target,secs,hits,miss,rate])))
  if phase=='rebuild':
   for suffix in ('cache-before.log','cache-after.log','cache-before.tsv','cache-after.tsv'):
    shutil.copyfile(f'build/scratch/{phase}/{target}.{suffix}',out/f'rebuild-{target}.{suffix}')
(out/'build-times.tsv').write_text('\n'.join(times)+'\n')
shutil.copyfile('build/scratch/pre/environment.log',out/'environment.log')
print('Evidence collected:',len(edits)-1,'edited source files; exact C++/R counts and 98 per-family quality results match.')
