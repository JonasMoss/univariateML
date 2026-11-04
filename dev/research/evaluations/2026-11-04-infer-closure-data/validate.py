"""Validate the retained research documents and record final run outcomes."""
import hashlib
import json
from pathlib import Path
import re
import subprocess

here = Path(__file__).resolve().parent
root = here.parents[3]
report = here.with_name('2026-11-04-infer-closure-report.md')
assert subprocess.check_output(['git','rev-parse','HEAD'], cwd=root, text=True).strip() == 'a3fbb01383ab1ec6065c43f7fade40b1bd2751a4'
assert not subprocess.check_output(['git','diff','--name-only'], cwd=root, text=True).strip()
subprocess.run(['git','diff','--check'], cwd=root, check=True)
files = sorted(p for p in here.iterdir() if p.is_file() and p.name != 'validation.json')+[report]
links = 0
for p in files:
    if p.suffix not in ('.md','.py','.sh','.json','.csv'):
        continue
    txt = p.read_text()
    assert all(line == line.rstrip() for line in txt.splitlines()), p
    if p.suffix == '.md':
        for target in re.findall(r'\]\(([^)]+)\)', txt):
            if target.startswith(('https:', 'http:', '#')):
                continue
            assert (p.parent/target.split('#')[0]).resolve().exists(), (p, target)
            links += 1
    if p.suffix == '.json':
        json.loads(txt)

expected = {
    'cpp-dev': '100% tests passed, 0 tests failed out of 106',
    'cpp-full': '100% tests passed, 0 tests failed out of 127',
    'r-dev': '[ FAIL 0 | WARN 0 | SKIP 154 | PASS 20612 ]',
    'r-full': '[ FAIL 0 | WARN 0 | SKIP 0 | PASS 43427 ]',
    'final-research': '"checks": "passed"',
}
for suffix, marker in expected.items():
    log = root/'build'/'scratch'/f'infer-closure-{suffix}.log'
    assert marker in log.read_text(), log
record = {'source_revision': 'a3fbb01383ab1ec6065c43f7fade40b1bd2751a4',
    'machine': 'local workstation; shared slot locks; nice -n 10',
    'preexisting_tracked_files_changed': [], 'local_markdown_links_checked': links,
    'whitespace_check': 'passed', 'json_check': 'passed', 'final_run_markers': expected,
    'sha256': {str(p.relative_to(root)): hashlib.sha256(p.read_bytes()).hexdigest() for p in files}}
(here/'validation.json').write_text(json.dumps(record, indent=2)+'\n')
print(f'Validated {len(files)} research files and {links} local Markdown links; final runs green.')
