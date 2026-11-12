"""Repeat only site validation after correcting dossier fragment links."""
import json, os, subprocess, time, sys
from pathlib import Path
H = Path(__file__).resolve().parent
ROOT = H.parents[3]
os.chdir(ROOT)
os.environ.update(TMPDIR=str(ROOT/'build/tmp'), JUST_TEMPDIR=str(ROOT/'build/tmp'),
                  XDG_RUNTIME_DIR=str(ROOT/'build/tmp'))
log = ROOT/'build/scratch/dpq-skew/final-logs/workstation-site-check-retry.log'
command = ['/tmp/umvl-cpu/run', 'just', 'site-check']
start = time.time()
with log.open('w') as out:
    code = subprocess.call(command, stdout=out, stderr=subprocess.STDOUT)
receipts = json.loads((H/'final-runs.json').read_text())
receipts.append(dict(label='workstation-site-check', machine='workstation',
                     command=command, exit_code=code, seconds=time.time()-start,
                     log=str(log.relative_to(ROOT)),
                     reason='Remove three dossier fragments stripped by catalogue rendering.'))
(H/'final-runs.json').write_text(json.dumps(receipts, indent=2)+'\n')
print('\n'.join(log.read_text(errors='replace').splitlines()[-20:]), flush=True)
sys.exit(code)
