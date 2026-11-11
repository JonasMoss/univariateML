"""Check final provenance/scope; remove the site check's unrelated capture drift."""
import hashlib,json,os,pathlib,re,shutil,subprocess
P=pathlib.Path(__file__).resolve().parent
os.environ['GIT_OPTIONAL_LOCKS']='0'
cache=P/'__pycache__'
if cache.exists():
 for path in cache.glob('oracle.*.pyc'):path.unlink()
 if not any(cache.iterdir()):cache.rmdir()
def git(*args):return subprocess.check_output(['git',*args])
def sha(data):return hashlib.sha256(data).hexdigest()
checks=json.loads((P/'final-checks.json').read_text())
assert len(checks)==7 and all(r['exit_code']==0 for r in checks.values())
assert len({r['source_sha256'] for r in checks.values()})==1
LOG=pathlib.Path('build/scratch/acc-fb-count-logs')
LOG.mkdir(parents=True,exist_ok=True)
# Keep raw logs in ignored scratch, and keep the checked receipts/counters here.
for name,row in checks.items():
 log=P/row['log'] if row['log'].startswith('logs/') else pathlib.Path(row['log'])
 if not log.exists():log=LOG/(name+'.log')
 contents=log.read_text(errors='replace')
 if name in ['test','test-full']:
  counts=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',contents)[-1]
  row['counts']=dict(zip(['fail','warn','skip','pass'],map(int,counts)))
  assert row['counts']['fail']==row['counts']['warn']==0
 elif name.startswith('cpp-test'):
  failed,total=re.findall(r'(\d+) tests failed out of (\d+)',contents)[-1]
  row['counts']=dict(failed=int(failed),total=int(total));assert int(failed)==0
 elif name=='js-test':
  row['counts']={key:int(re.findall(r'^# '+key+r' (\d+)$',contents,re.M)[-1]) for key in ['tests','pass','fail','skipped']}
  assert row['counts']['fail']==0
 row['log']=str(LOG/(name+'.log'))
(P/'final-checks.json').write_text(json.dumps(checks,indent=2)+'\n')
if (P/'logs').exists():
 for path in (P/'logs').iterdir():shutil.move(str(path),str(LOG/path.name))
 (P/'logs').rmdir()
first=P/'first-pass-checks.json'
if first.exists():
 rows=json.loads(first.read_text())
 for name,row in rows.items():row['log']=str(LOG/'first-pass'/(name+'.log.gz'))
 first.write_text(json.dumps(rows,indent=2)+'\n')
bindings=json.loads((P/'binding-checks.json').read_text())
assert len(bindings)==5 and all(r['exit_code']==0 for r in bindings.values())
spots=json.loads((P/'spot-checks.json').read_text())
assert all(r['n']==25 for r in spots.values())
families=['bbinom','hyper','nhyper','tbinom','ztbinom','zib','zmbinom','bnbinom','skellam']
paths=[]
for family in families:
 for base in ['univariateML-cpp/include/cppunivariate/distributions','bindings/common/distributions','bindings/r/distributions']:
  paths.extend(pathlib.Path(base,family).glob('*'))
paths.append(pathlib.Path('bindings/js/stop-test.mjs'))
digest=hashlib.sha256()
for path in sorted(p for p in paths if p.is_file() and p.suffix in ['.hpp','.cpp','.R','.md','.mjs']):
 digest.update(str(path).encode());digest.update(path.read_bytes())
assert digest.hexdigest()==checks['cpp-test']['source_sha256']
# site/src/components/theory/browser.mjs deliberately writes these historical
# captures. Keep this job's diff restricted to its count-family work.
capture=pathlib.Path('dev/research/evaluations/2026-11-02-viz-clip/after-mobile-held-face.png')
original=git('show','HEAD:'+str(capture))
capture_record=dict(path=str(capture),generated_sha256=sha(capture.read_bytes()),baseline_sha256=sha(original))
if capture.read_bytes()!=original:capture.write_bytes(original)
subprocess.run(['git','diff','--check'],check=True)
changed=git('diff','--name-only').decode().splitlines()
owned=['bbinom','tbinom','ztbinom','zib','zmbinom']
allowed=['bindings/js/stop-test.mjs']
for family in owned:
 allowed.extend([f'univariateML-cpp/include/cppunivariate/distributions/{family}/',f'bindings/r/distributions/{family}/',f'bindings/common/distributions/{family}/'])
assert all(any(p==a or p.startswith(a) for a in allowed) for p in changed),changed
new=git('ls-files','--others','--exclude-standard').decode().splitlines()
assert all(p.startswith(str(P.relative_to(pathlib.Path.cwd()))) or p=='dev/research/evaluations/2026-11-11-acc-fb-count-report.md' or p in [f'univariateML-cpp/include/cppunivariate/distributions/{f}/accuracy.hpp' for f in ['bbinom','tbinom','ztbinom']] for p in new),new
assert all(r['identical'] for r in json.loads((P/'unchanged-source.json').read_text()))
audit=dict(baseline_commit=git('rev-parse','HEAD').decode().strip(),machine='workstation',
           source_sha256=digest.hexdigest(),all_seven_checks_passed=True,
           binding_checks_passed=True,diff_check_passed=True,scope_check_passed=True,
           raw_logs_directory=str(LOG),
           restored_site_capture=capture_record,changed_tracked_files=changed,
           new_files=new,unchanged_dependency_files=len(json.loads((P/'unchanged-source.json').read_text())))
(P/'final-audit.json').write_text(json.dumps(audit,indent=2)+'\n')
files=[pathlib.Path(p) for p in changed+new if pathlib.Path(p).is_file()]
manifest={str(p):sha(p.read_bytes()) for p in sorted(set(files)) if p.name!='final-manifest.json'}
(P/'final-manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
print(json.dumps({k:v for k,v in audit.items() if k not in ['changed_tracked_files','new_files']},indent=2))
