"""Real working-tree demonstrations, with byte-for-byte restoration and no git writes."""
import contextlib
import importlib.util
import io
import os
from pathlib import Path
import subprocess
import sys

root = Path.cwd()
# This harness invokes the loaded runner directly while its recipe is temporarily
# absent. Mirror the Justfile's cache environment for those CMake calls.
os.environ['CCACHE_DIR'] = os.getenv('UMVL_CCACHE_DIR', str(Path.home()/'.cache/ccache/univariateML'))
os.environ['CCACHE_BASEDIR'] = str(root)
os.environ['CCACHE_NOHASHDIR'] = '1'
os.environ['CCACHE_MAXSIZE'] = '8G'
spec = importlib.util.spec_from_file_location('changed', root / 'dev/tools/test-changed.py')
changed = importlib.util.module_from_spec(spec)
spec.loader.exec_module(changed)
evidence = root / 'dev/research/evaluations/2026-11-11-test-scope-data'
evidence.mkdir(parents=True, exist_ok=True)
paths = set(filter(None, changed.git('diff', '--name-only', '-z', 'HEAD').split('\0')))
paths.update(filter(None, changed.git('ls-files', '--others', '--exclude-standard', '-z').split('\0')))
# Evidence from previous phases is not a source change and must stay put.
paths = {p for p in paths if not p.startswith('dev/research/evaluations/')}
saved = {p: (root / p).read_bytes() for p in paths}
modes = {p: (root / p).stat().st_mode & 0o777 for p in paths}
baseline = {}
for p in paths:
    result = subprocess.run(['git', 'show', f'HEAD:{p}'], capture_output=True)
    baseline[p] = result.stdout if result.returncode == 0 else None
mutated = {}
try:
    for p, data in baseline.items():
        if data is None:
            (root / p).unlink()
        else:
            (root / p).write_bytes(data)
    for name, path in [('leaf', str(changed.DOSSIERS / 'bhatt/test.cpp')),
                       ('delegate', str(changed.DOSSIERS / 'invweibull/mle.hpp')),
                       ('core', 'univariateML-cpp/include/cppunivariate/core/span.hpp')]:
        file = root / path
        original = file.read_bytes()
        mutated[path] = original
        file.write_bytes(original + b'\n// TASK-193 selection demonstration.\n')
        output = io.StringIO()
        sys.argv = ['test-changed.py', 'master', '--dry-run']
        with contextlib.redirect_stdout(output):
            assert changed.main() == 0
        (evidence / f'select-{name}.log').write_text(output.getvalue())
        selected, reasons, shared = changed.select(changed.changed_paths('master')[1])
        if name == 'leaf':
            assert selected == ['bhatt'], selected
        elif name == 'delegate':
            assert 'invweibull' in selected and 'frechet' in selected
        else:
            assert len(selected) == 99 and shared
        file.write_bytes(original)
        del mutated[path]
    path = str(changed.DOSSIERS / 'bhatt/test.cpp')
    file = root / path
    mutated[path] = file.read_bytes()
    file.write_bytes(mutated[path] + b'\nTEST_CASE("TASK-193 deliberately broken family test") { CHECK(false); }\n')
    # Execute the same runner from memory: its on-disk implementation has been
    # temporarily removed to leave only the real family test change in git diff.
    sys.argv = ['test-changed.py', 'master']
    with (evidence / 'broken-family.log').open('w') as output:
        with contextlib.redirect_stdout(output):
            # Subprocesses inherit fd 1, not redirect_stdout's Python wrapper.
            import os
            stdout = os.dup(1)
            stderr = os.dup(2)
            try:
                os.dup2(output.fileno(), 1)
                os.dup2(output.fileno(), 2)
                code = changed.main()
            finally:
                os.dup2(stdout, 1)
                os.dup2(stderr, 2)
                os.close(stdout)
                os.close(stderr)
    assert code != 0, 'Broken test was not caught'
    log = (evidence / 'broken-family.log').read_text()
    assert 'TASK-193 deliberately broken family test' in log, log[-3000:]
    (evidence / 'demonstrations.txt').write_text('Real working-tree edits; no git write commands.\nLeaf, reverse dependency, shared-core selection assertions passed.\nBroken family test detected; runner exit ' + str(code) + '.\nAll temporary source edits restored in finally.\n')
finally:
    for p, data in mutated.items():
        (root / p).write_bytes(data)
    for p, data in saved.items():
        (root / p).parent.mkdir(parents=True, exist_ok=True)
        (root / p).write_bytes(data)
        (root / p).chmod(modes[p])

# With infrastructure restored, exercise the complete family plan as well. The
# path list is injected here because this job may not commit its infrastructure;
# the preceding cases independently verified real git-derived selections.
actual_changed_paths = changed.changed_paths
ancestor = changed.git('merge-base', 'master', 'HEAD').strip()
changed.changed_paths = lambda base: (ancestor, [str(changed.DOSSIERS / 'bhatt/test.cpp')])
sys.argv = ['test-changed.py', 'master']
with (evidence / 'selected-family-green.log').open('w') as output:
    import os
    stdout = os.dup(1)
    stderr = os.dup(2)
    try:
        os.dup2(output.fileno(), 1)
        os.dup2(output.fileno(), 2)
        with contextlib.redirect_stdout(output):
            code = changed.main()
    finally:
        os.dup2(stdout, 1)
        os.dup2(stderr, 2)
        os.close(stdout)
        os.close(stderr)
        changed.changed_paths = actual_changed_paths
assert code == 0, 'Complete selected-family plan failed; see selected-family-green.log'
with (evidence / 'demonstrations.txt').open('a') as output:
    output.write('Complete selected-family C++/R/JS plan passed with the isolated leaf path list injected after restoring infrastructure.\n')
