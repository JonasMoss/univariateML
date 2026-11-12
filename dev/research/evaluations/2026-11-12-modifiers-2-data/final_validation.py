"""Persist final foreground-suite results after every required suite completes."""
from pathlib import Path
import datetime
import hashlib
import json
import re
import subprocess

root = Path.cwd()
dest = Path(__file__).resolve().parent
scratch = root / 'build/scratch'
ledger = (scratch / 'post-control-machines.txt').read_text().splitlines()
for task in ['cpp-test', 'cpp-test-no-long-double', 'test', 'test-full']:
    assert any(line.startswith(task + ' workstation 0') for line in ledger), task

checks = []
for task in ['cpp-test', 'cpp-test-no-long-double']:
    log = (scratch / ('post-control-' + task + '.log')).read_text()
    match = re.search(r'100% tests passed, 0 tests failed out of (\d+)', log)
    assert match, task
    checks.append(dict(command='just ' + task, machine='workstation', exit=0,
                       passed=int(match[1]), log_sha256=hashlib.sha256(log.encode()).hexdigest()))
for task in ['test', 'test-full']:
    log = (scratch / ('post-control-' + task + '.log')).read_text()
    matches = re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]', log)
    assert matches, task
    fail, warn, skip, passed = map(int, matches[-1])
    assert fail == warn == 0
    if task == 'test-full':
        assert skip == 0
    checks.append(dict(command='just ' + task, machine='workstation', exit=0,
                       passed=passed, skipped=skip, warnings=warn, failures=fail,
                       log_sha256=hashlib.sha256(log.encode()).hexdigest()))
log = (scratch / 'final-js-test.log').read_text()
assert '# tests 626\n' in log and '# pass 626\n' in log and '# fail 0\n' in log
checks.append(dict(command='just js-build; just js-test', machine='workstation',
                   exit=0, passed=626, log_sha256=hashlib.sha256(log.encode()).hexdigest()))
log = (scratch / 'final-site-check.log').read_text()
assert 'PASS: 99 generated C++ snippets compile and link as C++17' in log
assert 'Total dist: 26543139 B' in log
checks.append(dict(command='just site-check', machine='workstation', exit=0,
                   compiled_examples=99, log_sha256=hashlib.sha256(log.encode()).hexdigest()))
log = (scratch / 'final-relations.log').read_text()
assert '157 edges; 0 failed' in log
checks.append(dict(command='just relations', machine='workstation', exit=0,
                   records=157, note='Includes declared relations not numerically checked.'))
typescript = json.loads((dest / 'typescript.json').read_text())
assert typescript['after_final_build_exit'] == 0
checks.append(dict(command=typescript['command'], machine='workstation', exit=0))
changed = subprocess.check_output(['git', 'diff', '--name-only'], text=True).splitlines()
for name in changed:
    assert '/special/' not in name and not name.endswith(('/dpqr.hpp', '/truth.py', '/truth.json', '/golden.json'))
    assert name not in ['bindings/r/NEWS.md', 'dev/STATUS.md', 'dev/KNOWN_ISSUES.md']
    assert not name.startswith(('dev/roadmap/', 'bindings/r/scripts/codegen'))
source_hashes = {name: hashlib.sha256((root / name).read_bytes()).hexdigest() for name in changed}
name = 'univariateML-cpp/include/cppunivariate/distributions/zeta/truncation.hpp'
source_hashes[name] = hashlib.sha256((root / name).read_bytes()).hexdigest()
record = dict(baseline=subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
              recorded_at=datetime.datetime.now(datetime.timezone.utc).isoformat(),
              final_suites=checks, machines=ledger, source_sha256=source_hashes,
              protected_sources_unchanged=True,
              compiler=subprocess.check_output(['c++', '--version'], text=True).splitlines()[0],
              open_checks=['GCC 13 simbox unavailable (exit 75).',
                           'Optional GNU 15 -O2 isolated Burr/inverse-Burr baseline tail failures; see optimized-baseline.json.',
                           '45 pre-existing repository link findings; see source-checks.json.',
                           'Artifact viewer HTTP 403: this worktree is outside its readable roots.'])
(dest / 'final-validation.json').write_text(json.dumps(record, indent=2) + '\n')
print(json.dumps(checks, indent=2))
