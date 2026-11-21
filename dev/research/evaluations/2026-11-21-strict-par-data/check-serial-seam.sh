#!/usr/bin/env bash
set -euo pipefail
root=$PWD
scratch=$root/build/scratch
dest=$scratch/strict-serial-seam-final
mkdir -p "$dest"
export TMPDIR=$root/build/tmp JUST_TEMPDIR=$root/build/tmp XDG_RUNTIME_DIR=$root/build/tmp
export GIT_OPTIONAL_LOCKS=0 PYTHONDONTWRITEBYTECODE=1 PYTHONUNBUFFERED=1
export PYTHONPATH=$scratch/audit-hook:$root/dev/validation
export STRICT_RATCHET_DIR=$dest
/usr/bin/time -o "$dest/total.time" -f '%e %U %S %x' \
  python3 dev/validation/strict-seam.py native double --jobs 1 --no-cache \
  >"$dest/strict.log" 2>"$dest/strict.stderr"
for file in native double native-retry double-retry; do
  [[ ! -f build/strict/seam/$file.jsonl ]] || cp "build/strict/seam/$file.jsonl" "$dest/"
done
python3 - "$scratch" "$dest" <<'PY'
import difflib
from pathlib import Path
import runpy
import sys

scratch, dest = map(Path, sys.argv[1:])
check = runpy.run_path(str(scratch/'compare-strict.py'))
baseline = scratch/'strict-old'
for name, before, after in (
    ('seam', check['seam'](baseline), check['seam'](dest)),
    ('stdout', [line for line in check['output'](baseline)
                if line.startswith(('seam-check ', 'strict-seam: '))], check['output'](dest)),
    ('ratchet', check['lines'](baseline/'strict-seam.ratchet.json'), check['lines'](dest/'strict-seam.ratchet.json')),
    ('stderr', check['lines'](baseline/'strict.stderr'), check['lines'](dest/'strict.stderr')),
):
    diff = ''.join(difflib.unified_diff(before, after, 'old/'+name, 'serial-final/'+name))
    (dest/(name+'.diff')).write_text(diff)
    assert not diff, name
    print(name, len(before), 'identical lines')
PY
