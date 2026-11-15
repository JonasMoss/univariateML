"""Compare every truth.json with a git revision, ignoring the metadata fields.

Usage: python3 verify_payload.py [REV]   (default: master)
Metadata fields: top-level input_sha256 / density_sha256 and normalization
"seconds". Everything else must be byte-identical in the serialized text
(the files are compared after deleting those fields from the text, and again
as parsed JSON).
"""
import json
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
DISTS = ROOT/'univariateML-cpp/include/cppunivariate/distributions'
rev = sys.argv[1] if len(sys.argv) > 1 else 'master'


def strip(text):
    text = re.sub(r',\n      "seconds": [^\n]+', '', text)
    return re.sub(r'\n  "(?:density|input)_sha256": "[0-9a-f]+",?', '', text)


changed = same_fp = bad = 0
ids_now = {p.parent.name for p in DISTS.glob('*/truth.json')}
ids_old = set()
for path in sorted(DISTS.glob('*/truth.json')):
    rel = path.relative_to(ROOT).as_posix()
    old = subprocess.run(['git', 'show', f'{rev}:{rel}'], cwd=ROOT, capture_output=True, text=True)
    if old.returncode:
        print('NEW (not in rev):', path.parent.name); bad += 1; continue
    ids_old.add(path.parent.name)
    new = path.read_text()
    a, b = strip(old.stdout), strip(new)
    ja, jb = json.loads(old.stdout), json.loads(new)
    for j in (ja, jb):
        j.pop('input_sha256', None); j.pop('density_sha256', None)
        for r in j['normalization']: r.pop('seconds', None)
    if a != b or ja != jb:
        print('PAYLOAD DIFFERS:', path.parent.name); bad += 1
    if old.stdout != new: changed += 1
    else: same_fp += 1
print(f'{len(ids_now)} tables; fingerprint-only changes in {changed}, untouched files {same_fp}; payload mismatches/new {bad}')
sys.exit(1 if bad else 0)
