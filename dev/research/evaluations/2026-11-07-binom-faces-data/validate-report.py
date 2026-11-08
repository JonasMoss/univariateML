#!/usr/bin/env python3
"""Validate the delivered report's local links and tested production hashes."""
from pathlib import Path
import hashlib,json,re
report=Path('dev/research/evaluations/2026-11-07-binom-faces-report.md')
base=report.parent/'2026-11-07-binom-faces-data'
links=re.findall(r'\[[^\]]*\]\(([^)]+)\)',report.read_text())
assert links
for link in links:
    assert (report.parent/link.split('#',1)[0]).is_file(),link
manifest=json.loads((base/'source-manifest.json').read_text())
for name,digest in manifest.items():
    assert hashlib.sha256(Path(name).read_bytes()).hexdigest()==digest,name
for family,record in json.loads((base/'golden-integrity.json').read_text()).items():
    p=Path('univariateML-cpp/include/cppunivariate/distributions')/family/'golden.json'
    assert hashlib.sha256(p.read_bytes()).hexdigest()==record['sha256'],family
print(f'PASS: {len(links)} report links, {len(manifest)} production hashes and four golden hashes.')
