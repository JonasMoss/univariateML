#!/usr/bin/env python3
"""Run on one reserved slot after every final suite has finished green."""
import gzip, hashlib, importlib.util, io, json, re, shutil, subprocess
from pathlib import Path
from types import SimpleNamespace
ROOT=Path(__file__).resolve().parents[4]
D=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('snapshot',ROOT/'dev/validation/fit-snapshot.py')
fs=importlib.util.module_from_spec(spec);spec.loader.exec_module(fs)
with (D/'difference-counts.log').open('w') as out:
    subprocess.run(['python3',str(D/'classify-diffs.py')],cwd=ROOT,stdout=out,stderr=subprocess.STDOUT,check=True)
results={}
for line in (D/'suite-results.tsv').read_text().splitlines():
    suite,status,*_=line.split('\t');results[suite]=status
assert all(results.get(s)=='PASS' for s in ['cpp-test','cpp-test-no-long-double','test-full','js-build','js-test','site-check']),'final suite did not pass'
assert all(results.get(s)=='PASS' for s in ['cpp-invparalogis-targeted','cpp-double-invparalogis-targeted','r-dev-targeted']),'late checks did not pass'
if results.get('test')!='PASS':
    initial=(D/'test.log').read_text()
    failures=set(re.findall(r"(?:Failure|Error) \('([^']+)'\)",initial))
    assert failures=={'test-dist-binom.R:231:3','test-dist-binom.R:232:3','test-dist-invparalogis.R:43:3'},failures
    assert '[ FAIL 3 | WARN 0 | SKIP 157 | PASS 36228 ]' in initial
    (D/'late-recheck-resolution.json').write_text(json.dumps(dict(
        initial_dev='FAIL 3, WARN 0, SKIP 157, PASS 36228',
        resolved_by='r-dev-targeted: both affected files pass; test-full: complete final-source suite passes',
        files=['binom/test.R (CMB cross-family assertion)','invparalogis/test.R (unchanged range assertion)'],
        core_rechecks=['cpp-invparalogis-targeted','cpp-double-invparalogis-targeted']),indent=2)+'\n')
current=fs.fingerprint(SimpleNamespace(no_long_double=False,cpu_limit=300))
for mode in ['native','double']:
    folder=ROOT/'build/snapshots'/mode
    provenance=json.loads((folder/'provenance.json').read_text())
    assert provenance['source_sha256']==current['source_sha256'], (mode,'stale snapshot')
    shutil.copyfile(folder/'provenance.json',D/f'{mode}.provenance.json')
    rows=fs.load(folder)
    with (D/f'{mode}.jsonl.gz').open('wb') as binary:
        with gzip.GzipFile(filename='',fileobj=binary,mode='wb',mtime=0) as compressed:
            with io.TextIOWrapper(compressed) as text:
                for key in sorted(rows):text.write(json.dumps(rows[key],sort_keys=True)+'\n')
for source in (ROOT/'build/strict/seam').glob('*.json'):
    if source.name.endswith(('.summary.json','.provenance.json')):
        shutil.copyfile(source,D/('seam-'+source.name))
known=json.loads((ROOT/'dev/validation/strict-known.json').read_text())['known']
baseline=json.loads((D/'baseline-known.json').read_text())
families=set('beta bnbinom cmb dlaplace dweibull fatigue frechet genbeta gompertz hcauchy huber invexp invparalogis invweibull llogis logitnorm naka paralogis pareto3 power sgomp sn st trgamma weibull'.split())
protected={k:v for k,v in baseline.items() if v['ref'] in {'TASK-214.5','TASK-214.13'}}
assert all(known.get(k)==v for k,v in protected.items()),'protected entry changed'
assert not {k:v for k,v in known.items() if k.split('|')[2] in families and v['ref'] in {'TASK-214.15','TASK-214.16','TASK-214.17'}},'scoped entry remains'
(D/'retained-known.json').write_text(json.dumps({k:v for k,v in known.items() if k.split('|')[2] in families},indent=2)+'\n')
with (D/'scope-check.log').open('w') as out:
    subprocess.run(['git','diff','--check'],cwd=ROOT,stdout=out,stderr=subprocess.STDOUT,check=True)
    subprocess.run(['git','diff','--stat'],cwd=ROOT,stdout=out,check=True)
for source in sorted(D.iterdir()):
    if source.is_file() and source.suffix!='.gz' and (source.suffix=='.log' or source.stat().st_size>200000):
        with source.open('rb') as raw,(D/(source.name+'.gz')).open('wb') as binary:
            with gzip.GzipFile(filename='',fileobj=binary,mode='wb',mtime=0) as compressed:shutil.copyfileobj(raw,compressed)
        source.unlink()
manifest=dict(baseline='da2174a1',source_sha256=current['source_sha256'],compiler=current['compiler'],machine='workstation',files={})
for source in sorted(D.iterdir()):
    if source.is_file() and source.name!='manifest.json':
        manifest['files'][source.name]=dict(bytes=source.stat().st_size,sha256=hashlib.sha256(source.read_bytes()).hexdigest())
(D/'manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
print('Final snapshots, provenance, classifications, judge and suite logs packed; protected entries preserved.')
