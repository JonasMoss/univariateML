"""Bounded diagnostic for the separate libc++ gamma rejection-loop overflow.
Run from repository root after just js-build. The child is killed and reaped
on timeout; this diagnostic succeeds only for the documented failure pattern.
"""
import json, pathlib, subprocess
source = '''import create from './build/js/univariateML.mjs';
const m=await create();
console.log('before-finite');
console.log([...m.rsstd(1,[0,1,1e100,2],157)]);
console.log('before-largest');
console.log([...m.rsstd(1,[0,1,Number.MAX_VALUE,2],157)]);
console.log('after-largest');
'''
try:
    p=subprocess.run(['node','--v8-pool-size=1','--input-type=module','-e',source],capture_output=True,text=True,timeout=10)
    result={'timed_out':False,'returncode':p.returncode,'stdout':p.stdout,'stderr':p.stderr}
except subprocess.TimeoutExpired as e:
    stdout=e.stdout.decode() if isinstance(e.stdout,bytes) else e.stdout or ''
    result={'timed_out':True,'timeout_seconds':10,'stdout':stdout}
path=pathlib.Path(__file__).resolve().parent/'wasm-sampler-check.json'
path.write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result))
assert result['timed_out'] and 'before-largest' in result['stdout'] and 'after-largest' not in result['stdout']
