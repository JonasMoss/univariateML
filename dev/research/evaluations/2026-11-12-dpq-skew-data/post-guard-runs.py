"""Finish shared-guard validation after the original foreground driver exits."""
import json, os, subprocess, sys, time
from pathlib import Path
H = Path(__file__).resolve().parent
ROOT = H.parents[3]
os.chdir(ROOT)
os.environ.update(TMPDIR=str(ROOT/'build/tmp'), JUST_TEMPDIR=str(ROOT/'build/tmp'),
                  XDG_RUNTIME_DIR=str(ROOT/'build/tmp'))
for key in ['UMVL_TEST_FAMILIES', 'FIT_FAMILY', 'JS_FAMILIES', 'UMVL_JS_GROUP',
            'UMVL_JS_GROUP_COUNT', 'UMVL_TEST_TIER']:
    os.environ.pop(key, None)
prior = json.loads((H/'final-runs.json').read_text())
assert prior[-1]['label']=='workstation-site-check' and prior[-1]['exit_code']==0
W = ROOT/'build/scratch/dpq-skew/final-logs'
receipts = []
def run(label, command, machine):
    log = W/(label+'.log')
    print('START', label, machine, flush=True)
    start = time.time()
    with log.open('w') as out:
        code = subprocess.call(command, stdout=out, stderr=subprocess.STDOUT)
    receipts.append(dict(label=label, command=command, machine=machine,
                         exit_code=code, seconds=time.time()-start,
                         log=str(log.relative_to(ROOT))))
    (H/'post-guard-runs.json').write_text(json.dumps(receipts, indent=2)+'\n')
    print('END', label, 'exit', code, flush=True)
    print('\n'.join(log.read_text(errors='replace').splitlines()[-18:]), flush=True)
    return code
batch = str((H/'post-guard-batch.py').relative_to(ROOT))
code = run('final-simbox-guard', ['/tmp/umvl-cpu/simbox-run', 'env',
           'CMAKE_BUILD_PARALLEL_LEVEL=4', 'CTEST_PARALLEL_LEVEL=4', 'python3', batch], 'simbox')
if code==75:
    code = run('final-workstation-guard', ['/tmp/umvl-cpu/run4', 'python3', batch], 'workstation')
elif code==0:
    # GCC 15 double-only validation is mandatory even after a GCC 13 pass.
    code = run('final-workstation-no-long-double', ['/tmp/umvl-cpu/run4', 'just',
               'cpp-test-no-long-double'], 'workstation')
sys.exit(code)
