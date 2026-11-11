#!/usr/bin/env python3
"""Check actual TAP name multiplicities and truth diagnostics, not just totals."""
from pathlib import Path
from collections import Counter
import json
import re
import sys

out = Path(__file__).resolve().parent
log = Path(sys.argv[1] if len(sys.argv)>1 else 'build/scratch/final-js-workstation-4.log').read_text()
before = (out/'js-before-names.txt').read_text().splitlines()
after = [s[len('# Subtest: '):] for s in log.splitlines() if s.startswith('# Subtest: ')]
assert Counter(before) == Counter(after), (Counter(before)-Counter(after), Counter(after)-Counter(before))
assert '# fail 0\n' in log and '# tests 615\n' in log
truth_before = json.loads((out/'js-before-truth.json').read_text())
truth_after = json.loads(Path('build/scratch/truth-results.json').read_text())
assert truth_before == truth_after
(out/'js-after-names.txt').write_text('\n'.join(sorted(after))+'\n')
(out/'js-after.log').write_text(log)
(out/'js-after-truth.json').write_text(json.dumps(truth_after,indent=2)+'\n')
(out/'js-comparison.json').write_text(json.dumps({
    'names_before':len(before),'names_after':len(after),'name_multisets_identical':True,
    'truth_checks':truth_after['checks'],'truth_diagnostics_identical':True,
    'before_seconds':315.55,
    'after_seconds':float(re.findall(r'^real ([0-9.]+)$',log,re.M)[-1]),
},indent=2)+'\n')
print('Identical 615-name multisets and 248155 truth checks; zero failures.')
