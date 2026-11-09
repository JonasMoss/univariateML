"""Final report/source scope and link checks; invoke through the slot wrapper."""
import os
import json
from pathlib import Path
import subprocess

out = Path(__file__).parent
env = dict(os.environ, GIT_OPTIONAL_LOCKS='0')
subprocess.run(['python3', str(out / 'audit.py')], check=True, env=env)
assert json.loads((out / 'coverage-source.json').read_text()) == json.loads((out / 'source.json').read_text())
coverage = json.loads((out / 'coverage.json').read_text())
assert coverage['overall']['samples'] == 270
assert sum(coverage['overall']['counts'].values()) == 270
links = subprocess.run(['python3', 'dev/tools/check_links.py'],
                       capture_output=True, text=True, env=env)
assert links.returncode in (0, 1), links.stderr
text = links.stdout + links.stderr
Path('build/scratch/bnbinom-allfree/final-links.txt').write_text(text)
expected = {
    'dev/research/evaluations/2026-11-03-boundary-lit-report.md:5: ../boundary-points.pdf',
    'univariateML-cpp/include/cppunivariate/distributions/nbinom/README.md:234: 1+m/r',
    'univariateML-cpp/notes/inference.md:3: /inference/',
    'univariateML-cpp/notes/inference.md:95: /inference/',
}
assert set(text.splitlines()[1:]) == expected, text
assert '4 broken' in text.splitlines()[0], text
status = subprocess.check_output(['git', 'status', '--short', '--untracked-files=all'],
                                 text=True, env=env)
allowed = (
    'univariateML-cpp/include/cppunivariate/distributions/bnbinom/',
    'bindings/common/distributions/bnbinom/',
    'bindings/r/distributions/bnbinom/',
    'dev/research/evaluations/2026-11-08-bnbinom-allfree',
)
for line in status.splitlines():
    path = line[3:]
    assert path == 'bindings/js/test.mjs' or path.startswith(allowed), line
subprocess.run(['git', 'diff', '--check'], check=True, env=env)
message = ('Report and changed-family links: PASS; four unchanged repository findings.\n'
           'Coverage experiment: 270 complete outcomes; final production-source fingerprints match.\n'
           'Change scope, frozen inputs, append-only shared tests, source identity and whitespace: PASS.\n')
with (out / 'validation.txt').open('a') as stream:
    stream.write('\n' + message + text)
print(message + text)
