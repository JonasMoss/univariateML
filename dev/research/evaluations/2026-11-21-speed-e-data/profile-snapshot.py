"""Measure family wall/CPU without changing fit-snapshot's inputs or outputs."""
import csv
import importlib.util
from pathlib import Path
import resource
import subprocess
import sys
import threading
import time

script, table, *options = sys.argv[1:]
sys.argv = [script, *options]
root = Path(script).resolve().parents[2]
baseline = Path(table).name.startswith('snapshot-before-')
source = root/'build/scratch/master/dev/validation/fit-snapshot.py' if baseline else Path(script)
spec = importlib.util.spec_from_file_location('fit_snapshot', source)
snapshot = importlib.util.module_from_spec(spec)
spec.loader.exec_module(snapshot)
if baseline:
    # Run the baseline scheduler against the same headers and include paths.
    snapshot.ROOT = root
    snapshot.DISTS = root/'univariateML-cpp/include/cppunivariate/distributions'
    snapshot.COMMON = root/'bindings/common'
    snapshot.ARCHIVE = root/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz'
run = subprocess.run
run_family = snapshot.run_family
close = snapshot.Probe.close
local = threading.local()
output = Path(table)
scratch = output.with_suffix('.times')
scratch.mkdir(parents=True, exist_ok=True)
rows = []


def timed_subprocess(command, *args, **kwargs):
    if not getattr(local, 'family', None) or '-o' not in command:
        return run(command, *args, **kwargs)
    path = scratch / local.family
    result = run(['/usr/bin/time', '-o', str(path), '-f', '%e %U %S', *command], *args, **kwargs)
    local.compile_wall, user, system = map(float, path.read_text().split())
    local.compile_cpu = user + system
    return result


def timed_close(probe):
    try:
        probe_time = snapshot.cpu_seconds(probe.child.pid)
    except (FileNotFoundError, ProcessLookupError):
        probe_time = 0.
    local.probe_cpu += probe_time
    close(probe)


def timed_family(ident, *args):
    local.family = ident
    local.compile_wall = local.compile_cpu = local.probe_cpu = 0.
    started, cpu = time.monotonic(), time.thread_time()
    result = run_family(ident, *args)
    rows.append((ident, result[1], time.monotonic() - started,
                 time.thread_time() - cpu + local.compile_cpu + local.probe_cpu,
                 local.compile_wall, local.compile_cpu, local.probe_cpu))
    local.family = None
    return result


snapshot.run_family = timed_family
snapshot.Probe.close = timed_close
subprocess.run = timed_subprocess
started = time.monotonic()
snapshot.main()
children = resource.getrusage(resource.RUSAGE_CHILDREN)
parent = resource.getrusage(resource.RUSAGE_SELF)
print('total wall %.3f, total CPU %.3f' %
      (time.monotonic() - started, children.ru_utime + children.ru_stime + parent.ru_utime + parent.ru_stime))
staged = output.with_suffix('.csv.tmp')
with staged.open('w', newline='') as stream:
    writer = csv.writer(stream)
    writer.writerow(('family', 'records', 'wall', 'cpu', 'compile_wall', 'compile_cpu', 'probe_cpu'))
    writer.writerows(sorted(rows))
staged.replace(output)
