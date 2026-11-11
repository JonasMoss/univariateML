"""Archive concise output only after all foreground validation commands finish."""
import hashlib,json,re
from pathlib import Path
D=Path(__file__).resolve().parent;root=D.parents[3]
manifest=json.loads((D/'manifest.json').read_text())
for path,digest in manifest['source_sha256'].items():
 assert hashlib.sha256((root/path).read_bytes()).hexdigest()==digest,path
identity=json.loads((D/'site-wasm-identity.json').read_text())
assert identity['byte_identical'] and identity['tested_bundle_before_site_check']==identity['bundle_after_site_check']
for path,digest in identity['bundle_after_site_check'].items():
 assert hashlib.sha256((root/path).read_bytes()).hexdigest()==digest,path
commands=[
 ('cpp-dev','just cpp-test','cpp-dev-corrected.log','cpp'),
 ('r-dev','just test','r-dev-corrected.log','r'),
 ('cpp-full','UMVL_TEST_TIER=full just cpp-test','cpp-full-corrected.log','cpp'),
 ('r-full','just test-full','r-full-corrected.log','r'),
 ('cpp-double','just cpp-test-no-long-double','cpp-double-corrected.log','cpp'),
 ('js-build','just js-build','js-build-corrected.log','build'),
 ('js-test','just js-test','js-test-corrected.log','js'),
 ('site-check','just site-check','site-check-corrected.log','site')]
rows=[];excerpts=[]
for name,command,filename,kind in commands:
 text=(root/'build/scratch'/filename).read_text(errors='replace')
 text=re.sub(r'\x1b\[[0-9;]*[A-Za-z]','',text)
 row=dict(name=name,command=command,machine='workstation',exit_code=0)
 if kind=='cpp':
  passed=re.findall(r'100% tests passed, 0 tests failed out of (\d+)',text)
  assert passed and 'Total Test time (real)' in text,name
  row['passed_shards']=int(passed[-1])
 elif kind=='r':
  counts=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',text)
  assert counts and counts[-1][:2]==('0','0'),name
  assert re.search(r'\n\s*user\s+system\s+elapsed\s*\n\s*[\d.]+\s+[\d.]+\s+[\d.]+\s*$',text),name
  row.update(zip(['failures','warnings','skips','passed_assertions'],map(int,counts[-1])))
 elif kind=='js':
  assert re.search(r'^# fail 0$',text,re.M) and '# duration_ms' in text,name
  row['passed_tests']=int(re.findall(r'^# pass (\d+)$',text,re.M)[-1])
 elif kind=='build':assert 'Validated and generated d/p/q/r for 99 families.' in text,name
 else:
  assert 'PASS: 99 generated C++ snippets compile and link as C++17' in text,name
  assert 'Total dist:' in text,name
  row['checked_families']=99
 assert not re.search(r'^error: Recipe .*failed',text,re.M),name
 rows.append(row)
 excerpts.append(command+' (workstation)\n'+'\n'.join(text.splitlines()[-22:]))
(D/'final-tests.json').write_text(json.dumps(rows,indent=2)+'\n')
(D/'final-test-output.txt').write_text('\n\n'.join(excerpts)+'\n')
print('PASS: all eight final commands completed green; concise output and source hashes retained.')
