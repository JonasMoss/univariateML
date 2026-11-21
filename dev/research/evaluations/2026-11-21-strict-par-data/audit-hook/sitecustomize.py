"""Capture the complete strict ratchet inputs and verdicts during measurements."""
import json
import os
import sys
from pathlib import Path

if 'STRICT_RATCHET_DIR' in os.environ:
    import strict_common

    ratchet = strict_common.ratchet

    def capture(failures, judged, owns, args, label):
        known = json.loads(args.known.read_text())['known'] if args.known.exists() else {}
        mine = {key for key in known if owns(key.split('|')[0])}
        keys = failures.keys()
        data = dict(
            failures=failures,
            listed=sorted(keys & mine),
            new=sorted(keys - mine),
            fixed=sorted(key for key in mine - keys if tuple(key.split('|')[1:5]) in judged),
        )
        (Path(os.environ['STRICT_RATCHET_DIR'])/(label+'.ratchet.json')).write_text(
            json.dumps(data, sort_keys=True, indent=2)+'\n')
        return ratchet(failures, judged, owns, args, label)

    strict_common.ratchet = capture

if 'STRICT_SEAM_PROFILE' in os.environ and Path(sys.argv[0]).name == 'seam-check.py':
    import atexit
    import collections
    import resource
    import subprocess
    import time
    import seam_reference

    reference = seam_reference.reference
    subprocess_run = subprocess.run
    seam_totals = collections.defaultdict(lambda: dict(calls=0, wall_s=0., cpu_s=0.))

    def child_time():
        usage = resource.getrusage(resource.RUSAGE_CHILDREN)
        return usage.ru_utime + usage.ru_stime

    def measured(command, *args, **kwargs):
        if command[0] == 'ccache':
            group = 'compile'
        elif Path(command[0]).name.startswith('probe-'):
            group = 'fit and density subprocess'
        else:
            return subprocess_run(command, *args, **kwargs)
        wall_start = time.perf_counter()
        cpu_start = child_time()
        try:
            return subprocess_run(command, *args, **kwargs)
        finally:
            entry = seam_totals[group]
            entry['calls'] += 1
            entry['wall_s'] += time.perf_counter() - wall_start
            entry['cpu_s'] += child_time() - cpu_start

    def measured_reference(*args, **kwargs):
        wall_start = time.perf_counter()
        cpu_start = time.process_time()
        try:
            return reference(*args, **kwargs)
        finally:
            entry = seam_totals['independent reference']
            entry['calls'] += 1
            entry['wall_s'] += time.perf_counter() - wall_start
            entry['cpu_s'] += time.process_time() - cpu_start

    def write_seam_profile():
        dest = Path(sys.argv[sys.argv.index('--output') + 1])
        path = Path(os.environ['STRICT_SEAM_PROFILE'])/(dest.stem+'.seam-profile.json')
        path.write_text(json.dumps(seam_totals, sort_keys=True, indent=2)+'\n')

    subprocess.run = measured
    seam_reference.reference = measured_reference
    atexit.register(write_seam_profile)
