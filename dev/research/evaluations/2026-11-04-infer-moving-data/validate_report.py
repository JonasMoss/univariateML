"""Check the research handoff: schema-shaped proposals, links, hashes and scope."""
import hashlib
import json
import re
import subprocess
from pathlib import Path

import yaml

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
REPORT = HERE.parent/'2026-11-04-infer-moving-report.md'
text = REPORT.read_text()
results = json.loads((HERE/'results.json').read_text())
critical = json.loads((HERE/'critical-results.json').read_text())
assert results['revision'] == critical['revision'] == subprocess.check_output(
    ['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()
assert not results['quadrature_warnings']
assert len(results['checks']) == 152 and all(r['passed'] for r in results['checks'])
assert critical['assertions_passed'] == 8
assert critical['main_results_sha256'] == hashlib.sha256((HERE/'results.json').read_bytes()).hexdigest()
for name, result in [('check.py', results), ('critical.py', critical)]:
    assert hashlib.sha256((HERE/name).read_bytes()).hexdigest() == result['script_sha256']
for path, digest in results['source_sha256'].items():
    assert hashlib.sha256((ROOT/path).read_bytes()).hexdigest() == digest, path
blocks = re.findall(r'^```yaml\n(.*?)^```', text, re.M | re.S)
assert len(blocks) == 6
regime_count = 0
for block in blocks:
    data = yaml.safe_load(block)
    assert set(data) == {'inference_numeric'}
    regimes = data['inference_numeric']
    assert len(set(r['regime'] for r in regimes)) == len(regimes)
    for row in regimes:
        assert {'regime', 'available', 'condition', 'classification'} <= set(row)
        assert isinstance(row['available'], bool)
        if not row['available']:
            assert row.get('reason')
        path, anchor = row['classification'].split('#')
        assert (ROOT/path).resolve() == REPORT.resolve()
        assert anchor in ['tukey-lambda', 'ged', 'gpd-and-gev', 'two-piece-normal-snorm', 'huber']
    regime_count += len(regimes)
body = re.sub(r'^```.*?^```', '', text, flags=re.M | re.S)
links = re.findall(r'\]\(([^)]+)\)', body)
local_count = 0
for link in links:
    if re.match(r'\w+://', link) or link.startswith('#'):
        continue
    assert (REPORT.parent/link.split('#')[0]).exists(), link
    local_count += 1
status = subprocess.check_output(['git', 'status', '--porcelain', '--untracked-files=all'], cwd=ROOT, text=True)
allowed = [str(REPORT.relative_to(ROOT)), str(HERE.relative_to(ROOT))+'/']
for line in status.splitlines():
    path = line[3:]
    assert path == allowed[0] or path.startswith(allowed[1]), line
summary = dict(revision=results['revision'], numerical_assertions=152,
               critical_assertions=8, quadrature_warnings=0,
               proposed_family_lists=6, proposed_regime_rows=regime_count,
               local_links_checked=local_count, source_hashes_checked=len(results['source_sha256']),
               working_tree_scope='only requested report and evidence directory',
               cpp_dev=dict(passed=106, failed=0, elapsed_ctest_seconds=65.25, machine='workstation'),
               r_dev=dict(passed=20612, failed=0, warned=0, skipped=154,
                          skip_reason='requires full test tier', elapsed_runner_seconds=124.383,
                          machine='workstation'))
(HERE/'validation.json').write_text(json.dumps(summary, indent=2)+'\n')
print(json.dumps(summary, indent=2))
