import collections, gzip, json, sys
from pathlib import Path

fresh, stored, *master = map(Path, sys.argv[1:])
key = lambda r: (r['family'], r['source'], r['case'], r['input_sha256'])
def load(path):
    if path.is_dir():
        lines = (line for part in sorted(path.glob('*.jsonl')) for line in part.read_text().splitlines()[1:])
        return {key(r): r for r in map(json.loads, lines)}
    with gzip.open(path, 'rt') as lines:
        return {key(r): r for r in map(json.loads, lines)}

def deltas(old, new):
    moves = collections.Counter()
    for k in sorted(old.keys() | new.keys()):
        left, right = old.get(k), new.get(k)
        if left is None or right is None:
            moves[k[0], 'new' if left is None else 'removed', k[1]] += 1
            continue
        if left == right:
            continue
        a, b = left['result'], right['result']
        fields = sorted(f for f in a.keys() | b.keys() if a.get(f) != b.get(f))
        if a.get('kind') != b.get('kind'):
            fields += [str(a.get('kind'))+' -> '+str(b.get('kind'))]
        moves[k[0], 'changed', ', '.join(fields)] += 1
    return moves

new, old = load(fresh), load(stored)
changes = deltas(old, new)
if master:
    old_master = load(master[0])
    families = {k[0] for k in old_master}
    old = {k: v for k, v in old.items() if k[0] in families}
    current = {k: v for k, v in new.items() if k[0] in families}
    for name, scope_changes in [('stored -> master', deltas(old, old_master)),
                                ('master -> worktree', deltas(old_master, current))]:
        print(name)
        for row, n in sorted(scope_changes.items()):
            print(n, *row, sep='\t')
    for k in old_master.keys() & current.keys():
        a, b = old_master[k]['result'], current[k]['result']
        if a == b:
            continue
        payload = {'model', 'limit_estimates', 'candidates', 'message', 'outcome_message'}
        assert all(a.get(f) == b.get(f) for f in (a.keys() | b.keys())-payload), (k, a, b)
        assert len(a['candidates']) == len(b['candidates']), k
        for before, after in zip(a['candidates'], b['candidates']):
            assert all(before[n] == after[n] for n in ('loglik_lower', 'loglik_upper')), k
            assert all(after['estimates'].get(n) == v for n,v in before['estimates'].items()), k
        assert all(b['limit_estimates'].get(n) == v for n,v in a['limit_estimates'].items()), k
    assert old_master.keys() == current.keys()
else:
    for row, n in sorted(changes.items()):
        print(n, *row, sep='\t')
