"""Exercise one local-header change in an ignored baseline projection."""
import gzip
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys

worktree, projection = (Path(path).resolve() for path in sys.argv[1:])
script = projection/'dev/validation/seam-check.py'
original_script = script.read_bytes()
archive = projection/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz'
cases = json.loads(gzip.decompress(archive.read_bytes()))
first = 'exp'
second = next(family for family in ('unif', 'norm', 'pois')
              if hashlib.sha256(first.encode()).digest()[0] % 8 !=
              hashlib.sha256(family.encode()).digest()[0] % 8)
families = (first, second)
indices = [next(row['index'] for row in cases if row['family'] == family) for family in families]
folder = projection/'build/scratch/cache-controls'
folder.mkdir(parents=True, exist_ok=True)
cache = folder/'seam.sqlite3'
cache.unlink(missing_ok=True)
header = projection/f'bindings/common/distributions/{first}/fit.hpp'
source = header.read_bytes()


def run(stage):
    output = folder/(stage+'.jsonl')
    command = [sys.executable, str(script), '--family', ','.join(families), '--output', str(output),
               '--cache', str(cache), '--jobs', '4']
    for index in indices:
        command += ['--case', str(index)]
    text = subprocess.check_output(command, text=True, cwd=projection)
    count = re.search(r'^(seam-check: (\d+)/(\d+) probes cached)$', text, re.M)
    print(stage, *families, *indices, count[1], flush=True)
    return int(count[2]), output.read_bytes()


try:
    shutil.copyfile(worktree/'dev/validation/seam-check.py', script)
    cold, baseline = run('cold')
    warm, original = run('warm')
    header.write_bytes(source + b'\nnamespace umvl_binding { inline constexpr int seam_cache_control = 1; }\n')
    touched, changed = run('touched')
finally:
    header.write_bytes(source)
    script.write_bytes(original_script)
assert (cold, warm, touched) == (0, 2, 1)
assert baseline == original == changed
