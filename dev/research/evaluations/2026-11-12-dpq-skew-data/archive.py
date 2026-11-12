"""Seal successful final checks, compressed logs and source/binary fingerprints."""
import datetime, gzip, hashlib, json, os, subprocess
import study
H, ROOT, W = study.H, study.ROOT, study.W
main = json.loads((H/'final-runs.json').read_text())
post = json.loads((H/'post-guard-runs.json').read_text())
late = json.loads((H/'late-st-runs.json').read_text())
assert main[-1]['label']=='workstation-site-check' and main[-1]['exit_code']==0
assert post[-1]['exit_code']==0
assert late[-1]['exit_code']==0 and len(late)==2
for name in ['final-native','final-double','lean-js','final-js']:
    assert not json.loads((H/(name+'.json')).read_text())['failures'], name
for name in ['fit-preservation','fit-preservation-double']:
    result=json.loads((H/(name+'.json')).read_text())
    assert not result['changes'] and result['timeouts']==0, name
logs = H/'logs'; logs.mkdir(exist_ok=True)
paths = list((W/'final-logs').glob('*.log'))+[W/'sealed-current.log',W/'final-consumers.log']
for path in paths:
    (logs/(path.name+'.gz')).write_bytes(gzip.compress(path.read_bytes(), compresslevel=6, mtime=0))
receipts = []
for phase, items in [('initial-driver',main),('final-shared-guard',post),('late-family-st',late)]:
    for r in items:
        receipts.append(dict(r, phase=phase,
                             archived_log=str((logs/(ROOT/r['log']).name).relative_to(ROOT))+'.gz'))
(H/'validation-receipts.json').write_text(json.dumps(receipts,indent=2)+'\n')
base='217ab8b533cac25ccc8eba8d66eccde1b376f0b4'
env=dict(os.environ,GIT_OPTIONAL_LOCKS='0')
changed=subprocess.check_output(['git','diff','--name-only',base,'--'],cwd=ROOT,env=env,text=True).splitlines()
forbidden=['bindings/r/NEWS.md','dev/STATUS.md','dev/KNOWN_ISSUES.md']
assert not any(p in forbidden or p.startswith('dev/roadmap/') for p in changed)
headers=[ROOT/'univariateML-cpp/include/cppunivariate/special'/n for n in
         ['three_word.hpp','skew_coordinate.hpp','skew_t_angular.hpp','skew_probability.hpp']]
def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()
source={str(p.relative_to(ROOT)):digest(p) for p in [*[ROOT/p for p in changed],*headers]}
scripts={p.name:digest(p) for p in [*H.glob('*.py'),*H.glob('*.mjs')]}
evidence={p.name:digest(p) for p in H.glob('*.json') if p.name!='source-fingerprints.json'}
binaries={str(p.relative_to(ROOT)):digest(p) for p in
          [*[W/('probe-'+mode) for mode in ['native','double','baseline-native','baseline-double']],
           ROOT/'build/js/univariateML.wasm',ROOT/'build/js/univariateML.mjs',
           ROOT/'univariateML-cpp/build/tests/distribution_tests',
           ROOT/'build/cpp-no-long-double/tests/distribution_tests'] if p.exists()}
result=dict(baseline=base, recorded_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
            source=source, scripts=scripts, evidence=evidence, binaries=binaries)
(H/'source-fingerprints.json').write_text(json.dumps(result,indent=2)+'\n')
print('Sealed',len(paths),'logs;',len(source),'source files;',len(binaries),'binaries')
