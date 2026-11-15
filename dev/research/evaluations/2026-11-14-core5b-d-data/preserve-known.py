"""Keep out-of-scope baseline entries unchanged, including load-sensitive ones.
Only previously known, scoped TASK-214.15--18 entries may leave in this job.
No entry absent from the original allowlist is admitted.
"""
import json,pathlib,subprocess
D=pathlib.Path(__file__).resolve().parent
path=pathlib.Path('dev/validation/strict-known.json')
base=json.loads((D/'strict-known-original.json').read_text())
assert base==json.loads(subprocess.check_output(['git','show','da2174a11af7d31c47434a54a65d4108c6b00388:dev/validation/strict-known.json'])), 'Baseline allowlist must equal starting commit'
current=json.loads(path.read_text())
F=set('pois binom nbinom geom zeta lgser norm exp gamma laplace'.split())
def target(k,v):
 return k.split('|')[2].split('+')[0].strip() in F and v['ref'] in {'TASK-214.15','TASK-214.16','TASK-214.17','TASK-214.18'}
assert not set(current['known'])-set(base['known']), 'New known entries are forbidden'
kept={k:v for k,v in base['known'].items() if not target(k,v)}
kept.update({k:v for k,v in current['known'].items() if target(k,v)})
current['known']=dict(sorted(kept.items()))
path.write_text(json.dumps(current,indent=1)+'\n')
print('Removed scoped entries:',len(base['known'])-len(kept))
print('Remaining required entries:',sum(target(k,v) for k,v in kept.items()))
