#!/usr/bin/env python3
"""Fail if a retired public ID remains in a generated registry or help route."""
from pathlib import Path
import json,re
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
retired=set(json.loads((ROOT/'bindings/common/modifier-laws.json').read_text()))
D=ROOT/'univariateML-cpp/include/cppunivariate/distributions'
assert not any((D/id).exists() for id in retired)
ns=(ROOT/'univariateML-r/NAMESPACE').read_text()
assert not any('export(ml'+id+')' in ns for id in retired)
assert not any((ROOT/'univariateML-r/man'/('ml'+id+'.Rd')).exists() for id in retired)
exports=(ROOT/'build/js/generated/exports.hpp').read_text()
fit_exports=(ROOT/'build/js/generated/fit_exports.hpp').read_text()
for id in retired:
 assert not any('"'+kind+id+'"' in exports for kind in 'dpqr')
 assert '"fit'+id+'"' not in fit_exports
js=(ROOT/'bindings/js/families.mjs').read_text()
public=json.loads(re.search(r'export const families = (.*?);',js)[1])
fitted=json.loads(re.search(r'export const fitFamilies = (.*?);',js)[1])
assert not retired.intersection(public)
assert len(public)==86 and len(fitted)==85
assert not any(re.search(r'\b'+id+r':\s*Family', (ROOT/'bindings/js/family-types.d.ts').read_text()) for id in retired)
site=json.loads((ROOT/'site/src/data/families.json').read_text())
assert not retired.intersection(f['id'] for f in site)
assert len(site)==85
routes=json.loads((HERE.parent/'2026-11-12-modifiers-2-data/removal.json').read_text())['site_routes']
for route in routes:assert not (ROOT/'site/dist'/route.strip('/')/'index.html').exists(),route
for id in retired:
 for path in [ROOT/'site/dist/distributions'/id,ROOT/'site/dist/families'/id]:assert not path.exists(),path
result={'families':86,'fitted':85,'retired_public_ids':0,'R_wrappers_exports_help':0,'JS_registrations_types':0,'site_catalogue':len(site)}
(HERE/'public-surface-check.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result))
