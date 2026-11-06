"""Check all paired fixtures and print median certificate/proposal timings."""
import csv
import pathlib
import statistics
root = pathlib.Path(__file__).parent
sets = {}
for version in ('before', 'after'):
    sets[version] = []
    for i in range(1, 4):
        rows = list(csv.DictReader((root / f'{version}-paired-{i}.tsv').open(), delimiter='\t'))
        assert len(rows) == 23
        assert all(r['status'] == '0' and r['certified'] == '1' and r['budget'] == '0' for r in rows)
        sets[version].append({r['case']: r for r in rows})
for name in sets['before'][0]:
    for i in range(3):
        old, new = (sets[v][i][name] for v in ('before', 'after'))
        for field in ('shape', 'scale'):
            assert abs(float(old[field])/float(new[field])-1) < 1e-7, (name,field)
        assert abs(float(old['logLik'])-float(new['logLik'])) < 1e-7*(1+abs(float(old['logLik'])))
print('case\tbefore_certificate_ms\tafter_certificate_ms\tspeedup\tbefore_proposal_ms\tafter_proposal_ms')
for name in ('distinct-500', 'distinct-10000', 'distinct-100000', 'replication-100000'):
    med = lambda v,f: statistics.median(float(r[name][f]) for r in sets[v])
    old,new = (med(v,'certificate_ms') for v in ('before','after'))
    print(f'{name}\t{old:.6f}\t{new:.6f}\t{old/new:.3f}\t{med("before","proposal_ms"):.6f}\t{med("after","proposal_ms"):.6f}')
