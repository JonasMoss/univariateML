"""Replay frozen-result differences with the original snapshot serializer."""
from collections import defaultdict
import gzip
import json

from check import BASELINES, SCRATCH, fs, load, old_driver


def main():
    archive = defaultdict(list)
    for row in json.loads(gzip.decompress(fs.ARCHIVE.read_bytes())):
        archive[row['family']].append(row)
    route_ids = {value['id']: ident for ident, value in fs.ROUTES.items()}
    old = old_driver()
    boost = old.boost_include()
    for mode in ('native', 'double'):
        folders = [SCRATCH/f'{stem}-{mode}' for stem in ('current', 'routes')]
        if mode == 'native':
            folders.insert(0, SCRATCH/'native')
        fresh = load(','.join(map(str, folders)))
        frozen = load(BASELINES/f'{mode}.jsonl.gz')
        frozen.update(load(BASELINES/f'routes-{mode}.jsonl.gz'))
        changes = defaultdict(list)
        for key in sorted(fresh.keys() & frozen.keys()):
            if fresh[key]['result'] != frozen[key]['result']:
                family, source, label = key
                changes[family].append((source, label, fresh[key]['result']))
        work = SCRATCH/f'legacy-{mode}'
        work.mkdir(exist_ok=True)
        for family, differing in changes.items():
            ident = route_ids.get(family, family)
            old.ROUTE_MODE = family in route_ids
            meta, _ = old.read(ident)
            names = [p['name'] for p in meta['params']]
            cases = {(c['source'], c['case']): c for c in old.cases(ident, archive)}
            probe = old.Probe(old.build(ident, work, mode == 'double', boost))
            try:
                for source, label, expected in differing:
                    answer = probe.ask(old.line(cases[source, label], names), 300)
                    assert answer == expected, (mode, family, source, label)
            finally:
                probe.close()
            print(mode, family, len(differing), 'raw differences reproduced by the original driver')


if __name__ == '__main__':
    main()
