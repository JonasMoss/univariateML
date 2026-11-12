"""Recheck final headers; retain the already-frozen baseline fit records."""
import subprocess, os, json, collections
import study
H = study.H
subprocess.run(['python3', str(H / 'final-evidence.py')],
               env=dict(os.environ, DPQ_SKEW_SKIP_JS='1'), check=True)
os.environ['DPQ_SKEW_FIT_MODE'] = 'double'
study.replay('after-double')
before = json.loads((H / 'before-double-fits.json').read_text())
after = json.loads((H / 'after-double-fits.json').read_text())
assert [r['id'] for r in before] == [r['id'] for r in after]
changes = [dict(id=a['id'], before=a['result'], after=b['result'])
           for a, b in zip(before, after) if a['result'] != b['result']]
result = dict(checks=len(after), families=dict(collections.Counter(r['id'][0] for r in after)),
              changes=changes, timeouts=sum(r['result']=='timeout' for r in after),
              baseline='217ab8b533cac25ccc8eba8d66eccde1b376f0b4')
(H / 'fit-preservation-double.json').write_text(json.dumps(result, indent=2)+'\n')
assert not changes and not result['timeouts']
for script in ['oracle-points.py', 'lean-wasm.py']:
    subprocess.run(['python3', str(H / script)], check=True)
print('Final native, double-only, lean WASM and both fit replays green', flush=True)
