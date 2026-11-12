"""Read-only reference, fitter and golden preservation against the frozen commit."""
import collections, hashlib, json, subprocess
import study
BASE = '217ab8b533cac25ccc8eba8d66eccde1b376f0b4'
def original(path):
    return subprocess.check_output(['git', 'show', BASE+':'+str(path.relative_to(study.ROOT))], cwd=study.ROOT)
def inventory(doc):
    rows = []
    for kind in ['values', 'quantiles']:
        for r in doc[kind]+[e['row'] for e in doc.get('excluded_'+kind, [])]:
            rows.append((kind, json.dumps(doc['parameters'][r[0]]), json.dumps(r[1:])))
        for p in doc.get('excluded_parameter_points', []):
            rows.extend((kind, json.dumps(p['parameters']), json.dumps(r[1:])) for r in p[kind])
    return collections.Counter(rows)
records = []
for family in ['ged', 'sged', 'sstd', 'st']:
    path = study.D/family/'truth.json'
    old = json.loads(original(path)); new = json.loads(path.read_text())
    assert inventory(old)==inventory(new), family
    assert not any(new.get(k) for k in ['excluded_values','excluded_quantiles','excluded_parameter_points'])
    archived = old.get('excluded_parameter_points', [])
    moved = {k:len(old.get('excluded_'+k, [])) for k in ['values','quantiles']}
    archived_rows = {k:sum(len(p[k]) for p in archived) for k in ['values','quantiles']}
    records.append(dict(family=family, baseline=BASE,
                        lifted_rows={k:moved[k]+archived_rows[k] for k in moved},
                        row_exclusions_lifted=moved, archived_rows_lifted=archived_rows,
                        archived_members_lifted=len(archived),
                        parameter_points_before=len(old['parameters']),
                        parameter_points_after=len(new['parameters']),
                        reference_rows_unchanged=True))
(study.H/'reference-preservation.json').write_text(json.dumps(records, indent=2)+'\n')
sources = []
for family in study.F:
    for name in ['mle.hpp', 'golden.json']:
        path=study.D/family/name
        assert path.read_bytes()==original(path), str(path)
        sources.append(dict(path=str(path.relative_to(study.ROOT)), sha256=hashlib.sha256(path.read_bytes()).hexdigest()))
(study.H/'source-preservation.json').write_text(json.dumps(dict(baseline=BASE, unchanged=sources), indent=2)+'\n')
print('Reference rows unchanged; reinstated', {k:sum(r['lifted_rows'][k] for r in records) for k in ['values','quantiles']})
print('Five fitter headers and five goldens byte-identical to', BASE)
