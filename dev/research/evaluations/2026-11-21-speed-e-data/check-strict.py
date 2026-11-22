"""Verify the warm strict runs and the refinement work-budget exclusions."""
import csv
import hashlib
import importlib.util
import json
from pathlib import Path
import sys

data, scratch = (Path(path).resolve() for path in sys.argv[1:])
tool = data.parent/'2026-11-21-strict-par-data/compare-strict.py'
spec = importlib.util.spec_from_file_location('compare_strict', tool)
compare = importlib.util.module_from_spec(spec)
spec.loader.exec_module(compare)


def verify(name, before, after):
    assert before == after, name
    print(f'{name}: {len(before)} entries, SHA-256 {hashlib.sha256("".join(before).encode()).hexdigest()}')


before, after = (scratch/f'warm-strict-{phase}' for phase in ('before', 'after'))
verify('strict ratchet', compare.ratchet(before), compare.ratchet(after))
verify('certificate records', compare.replay(before), compare.replay(after))
verify('seam records after guard retries',
       compare.seam(scratch/'master/build/strict/seam'), compare.seam(scratch.parent/'strict/seam'))
before, after = (scratch/f'strict-{phase}' for phase in ('before', 'after'))
verify('full refinement records', compare.lines(before/'refine-full.jsonl'),
       compare.lines(after/'refine-full.jsonl'))
new = compare.lines(after/'refine-cache-only.jsonl')
budget = [json.loads(line) for line in new if 'not attempted: ' in line]
verify('cache-only refinement records apart from the budget notes',
       compare.lines(before/'refine-cache-only.jsonl'),
       [line for line in new if 'not attempted: ' not in line])
assert len(budget) == 26 and all(r['status'] == 'open' for r in budget)
with (data/'refine-budget-open.csv').open('w', newline='') as stream:
    writer = csv.DictWriter(stream, fieldnames=list(budget[0]))
    writer.writeheader()
    writer.writerows(budget)
print(f'refinement work-budget exclusions: {len(budget)}')
