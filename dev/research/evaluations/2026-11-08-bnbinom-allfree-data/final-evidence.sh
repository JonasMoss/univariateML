#!/bin/sh
# Invoke through run: all stages use one thread and no nested reservations.
set -eu
export TMPDIR="$PWD/build/tmp"
evidence=dev/research/evaluations/2026-11-08-bnbinom-allfree-data
sh "$evidence/joint-targeted.sh" > "$evidence/joint.json"
sh "$evidence/profile-targeted.sh" > "$evidence/profile.json"
python3 "$evidence/audit.py"
if python3 dev/tools/check_links.py > build/scratch/bnbinom-allfree/links.txt; then
  echo "Repository links: PASS"
else
  python3 - <<'PYTHON'
from pathlib import Path
s=Path('build/scratch/bnbinom-allfree/links.txt').read_text()
assert 'distributions/bnbinom/' not in s,s
assert '2026-11-08-bnbinom-allfree' not in s,s
print('Changed-family and evaluation links: PASS; pre-existing repository findings:')
print(s)
PYTHON
fi
