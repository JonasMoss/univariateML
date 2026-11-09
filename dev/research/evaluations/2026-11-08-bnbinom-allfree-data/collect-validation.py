"""Run after the final suite launchers have returned successfully."""
import json
import re
from pathlib import Path

out = Path(__file__).parent
scratch = Path('build/scratch/bnbinom-allfree')
required = {
    'final-cpp-dev.log': [('simbox', 'cpp-dev')],
    'final-cpp-full.log': [('simbox', 'cpp-full')],
    'final-cpp-no-long-double.log': [('workstation', 'cpp-no-long-double')],
    'final-r-dev.log': [('simbox', 'test')],
    'final-r-full.log': [('simbox', 'test-full')],
    'final-simbox-no-long-double.log': [('simbox', 'cpp-no-long-double')],
    'js-build-final.log': [('workstation', 'js-build')],
    'js-test-final.log': [('workstation', 'js-test')],
    'site-final-current.log': [('workstation', 'site-check')],
}
lines = ['Final validation results. The launchers in this directory document CPU reservations.',
         'Final runs after restoring the explicitly uncertified legacy fallback; earlier runs are superseded.']
for name, suites in required.items():
    text = re.sub(r'\x1b\[[0-9;]*[A-Za-z]', '',
                  (scratch / name).read_text(errors='replace'))
    lines.append('\n' + name)
    for machine, suite in suites:
        start = text.index(f'START {machine}-{suite} ')
        end = text.index(f'GREEN {machine}-{suite} ', start)
        block = text[start:end]
        summaries = re.findall(r'\[ FAIL \d+ \| WARN \d+ \| SKIP \d+ \| PASS \d+ \]', block)
        if summaries:
            assert summaries[-1].startswith('[ FAIL 0 | WARN 0 |'), summaries[-1]
            lines.append(suite + ': ' + summaries[-1])
        for line in block.splitlines():
            if line.startswith(('START ', '100% tests passed', '# tests ', '# pass ', '# fail ', 'PASS ', 'PASS:')):
                lines.append(line)
        lines.append(text[end:].splitlines()[0])
for name in ('native-targeted.txt', 'no-long-double-targeted.txt'):
    text = (out / name).read_text()
    assert 'Status: SUCCESS!' in text
    lines.extend(['\n' + name, text.strip()])
for name in ('oracle.json', 'timings.json', 'profile.json', 'joint.json',
             'diagonal-probe.json', 'root-probe.json', 'r-targeted.json', 'source.json',
             'compound-poisson.json', 'coverage.json', 'coverage-source.json'):
    json.loads((out / name).read_text())
    lines.append(name + ': valid JSON')
(out / 'validation.txt').write_text('\n'.join(lines) + '\n')
print('\n'.join(lines))
