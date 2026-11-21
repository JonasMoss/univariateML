import collections
import csv
import os
import runpy
import sys
import time

path = sys.argv.pop(1)
sys.argv[0] = path
sys.path.insert(0, os.path.dirname(path))
replay = runpy.run_path(path)
totals = collections.defaultdict(lambda: [0, 0.0])
inner = collections.defaultdict(lambda: [0, 0.0])
current_family = None
registry = replay['REPLAYS']
combined = replay['combined']

def measure(group, fn):
    def wrapped(record, data, budget):
        global current_family
        current_family = record['family']
        start = time.process_time()
        result = fn(record, data, budget)
        entry = totals[record['family'], group]
        entry[0] += 1
        entry[1] += time.process_time() - start
        return result
    return wrapped

def measure_cover_kernel(fn):
    def wrapped(*args, **kwargs):
        start = time.process_time()
        result = fn(*args, **kwargs)
        entry = inner[current_family, fn.__name__]
        entry[0] += 1
        entry[1] += time.process_time() - start
        return result
    return wrapped

globals_ = replay['replay_lomax'].__globals__
globals_['lomax_bnb'] = measure_cover_kernel(globals_['lomax_bnb'])
covers = replay['cert_replay_covers']
covers.cover = measure_cover_kernel(covers.cover)

base = {'nbinom': measure('C/D', replay['replay_nbinom']),
        'lomax': measure('C/E/F', replay['replay_lomax'])}
base.update({k: measure('B/C', fn) for k, fn in replay['cert_replay_roots'].REPLAYS.items()})
for module, group in [('faces', 'D'), ('covers', 'E/F'), ('integer', 'G/H')]:
    for k, fn in replay['cert_replay_' + module].REPLAYS.items():
        measured = measure(group, fn)
        base[k] = combined(base[k], measured) if k in base else measured
assert set(registry) == set(base)
registry.update(base)
try:
    replay['main']()
finally:
    with open(os.environ['STRICT_PROFILE'], 'w', newline='') as out:
        writer = csv.writer(out)
        writer.writerow(['family', 'mechanisms', 'records', 'cpu_s'])
        for (family, group), (count, seconds) in sorted(totals.items()):
            writer.writerow([family, group, count, f'{seconds:.6f}'])
    with open(os.environ['STRICT_PROFILE']+'.covers.csv', 'w', newline='') as out:
        writer = csv.writer(out)
        writer.writerow(['family', 'cover_kernel', 'calls', 'cpu_s_inclusive'])
        for (family, kernel), (count, seconds) in sorted(inner.items()):
            writer.writerow([family, kernel, count, f'{seconds:.6f}'])
