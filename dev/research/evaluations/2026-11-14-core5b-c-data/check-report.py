"""Check the completed report's local links, source provenance and evidence markers."""
import gzip,importlib.util,json,re,types
from pathlib import Path
D=Path(__file__).resolve().parent;root=D.parents[3]
def read(name):
 p=D/name
 if p.exists():return p.read_text()
 return gzip.decompress((D/(name+'.gz')).read_bytes()).decode()
assert read('cpp-log-collapse.log').count('100% tests passed, 0 tests failed out of 4')==2
for name,n in [('R-final-outcomes.log',587),('R-log-collapse.log',1645)]:
 assert f'[ FAIL 0 | WARN 0 | SKIP 0 | PASS {n} ]' in read(name)
assert '# pass 583\n# fail 0' in read('js-final-test.log')
assert 'PASS: 85 generated C++ snippets compile and link as C++17' in read('site-final-check.log')
assert 'strict layer: no failure outside the known list' in read('strict.log')
sp=importlib.util.spec_from_file_location('fs',root/'dev/validation/fit-snapshot.py');fs=importlib.util.module_from_spec(sp);sp.loader.exec_module(fs)
for mode in ['native','double']:
 saved=json.loads(read(mode+'-provenance.json'))
 current=fs.fingerprint(types.SimpleNamespace(no_long_double=mode=='double',cpu_limit=300))
 assert saved['source_sha256']==current['source_sha256'],mode
report=D.with_name('2026-11-14-core5b-c-report.md');links=[]
for target in re.findall(r'\]\(([^)]+)\)',report.read_text()):
 if '://' in target or target.startswith('#'):continue
 p=(report.parent/target.split('#')[0]).resolve()
 assert p.exists(),target
 links.append(target)
assert not [p.name for p in D.iterdir() if p.is_file() and p.suffix!='.gz' and p.stat().st_size>200000]
result=dict(local_links_checked=len(links),source_fingerprints_current=True,evidence_markers_passed=True,
 report=str(report.relative_to(root)),large_plain_evidence_files=[])
(D/'report-check.json').write_text(json.dumps(result,indent=2)+'\n')
print('Report verified:',len(links),'local links; current source fingerprints; final checks green.')
