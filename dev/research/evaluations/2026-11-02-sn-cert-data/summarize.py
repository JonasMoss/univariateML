"""Summarize old/new 600-fit outputs; files themselves stay in build/scratch."""
import collections
import math
import sys

def read(path):
    rows = {}
    with open(path) as f:
        for line in f:
            i, status, alpha, ll = line.split()
            rows[int(i)] = (int(status), float(alpha), float(ll))
    return rows

old, new = map(read, sys.argv[1:])
assert list(old) == list(new) == list(range(600))
print('old status counts', dict(sorted(collections.Counter(v[0] for v in old.values()).items())))
print('new status counts', dict(sorted(collections.Counter(v[0] for v in new.values()).items())))
changes = [i for i in old if old[i][0] != new[i][0]]
print('status changes', changes)
pairs = [(i, new[i][2] - old[i][2]) for i in old if old[i][0] == new[i][0] == 0]
print('finite paired fits', len(pairs))
print('largest absolute likelihood difference', max(pairs, key=lambda p: abs(p[1])))
print('likelihood gaps over 1e-6', [(i, d) for i, d in pairs if abs(d) > 1e-6])
print('new failures', [i for i in new if new[i][0] in (1, 2)])
