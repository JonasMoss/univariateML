"""Shared pieces of the strict layer (dev/design/consolidation.md, step 6).

Every strict check judges stored fit snapshots (fit-snapshot.py) and compares
its failures with one tracked list, dev/validation/strict-known.json. A known
entry is keyed 'check|mode|family|source|case' and names a card. Each script
owns some checks: it reports new failures of those checks, and only those
checks' listed entries can be pruned by it.
"""
import gzip, importlib.util, json, math, re, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
ARCHIVE = ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz'
KNOWN = HERE/'strict-known.json'
NOTE = ('Known failures of the strict layer (fit-judge.py, fit-density.py, fit-consistency.py, '
        'fit-refine.py, strict-seam.py), keyed check|mode|family|source|case. Entries leave when '
        'fixed (--prune); adding one (--add) is a reviewed decision with a card.')

_snapshot = None
def snapshot_module():
    """fit-snapshot.py as a module: its cases(), classes(), read() and line() rebuild every input."""
    global _snapshot
    if _snapshot is None:
        spec = importlib.util.spec_from_file_location('fit_snapshot', HERE/'fit-snapshot.py')
        _snapshot = importlib.util.module_from_spec(spec); spec.loader.exec_module(_snapshot)
    return _snapshot

def number(text):
    return float(text) if text in ('nan', 'inf', '-inf') else float.fromhex(text)

def load_one(path):
    rows = {}
    path = Path(path)
    if path.is_file():
        with gzip.open(path, 'rt') as source:
            for text in source:
                r = json.loads(text); rows[(r['family'], r['source'], r['case'])] = r
        return rows
    for part in sorted(path.glob('*.jsonl')):
        for text in part.read_text().splitlines()[1:]:
            r = json.loads(text); rows[(r['family'], r['source'], r['case'])] = r
    return rows

def load(paths):
    rows = {}
    for path in str(paths).split(','): rows.update(load_one(path))
    return rows

def provenance(path):
    path = Path(path)
    if path.is_dir(): return json.loads((path/'provenance.json').read_text())
    name = re.sub(r'\.jsonl(\.gz)?$', '', path.name)
    return json.loads((path.parent/(name+'.provenance.json')).read_text())

def mode_of(paths):
    """'native' or 'double', from the snapshots' provenance; all parts must agree."""
    modes = {'double' if provenance(p).get('no_long_double') else 'native' for p in str(paths).split(',')}
    if len(modes) != 1: raise SystemExit('snapshots %s mix precision modes' % paths)
    return modes.pop()

def archive_data():
    with gzip.open(ARCHIVE, 'rt') as source:
        return {c['index']: tuple(c['data']) for c in json.load(source)}

def add_arguments(ap):
    ap.add_argument('--known', type=Path, default=KNOWN)
    ap.add_argument('--prune', action='store_true', help='drop listed failures of these checks that no longer occur')
    ap.add_argument('--add', metavar='REF', help='list the new failures under this card (deliberate, reviewed)')
    ap.add_argument('--match', metavar='REGEX', help='with --add: list only the new failures whose key matches')
    ap.add_argument('--finding', metavar='SLUG', help='with --add: a short name grouping the entries (for a NEW card)')

def ratchet(failures, judged, owns, args, label):
    """Compare failures with the known list; return the exit code.

    failures: {key: detail}; judged: set of (mode, family, source, case) this run
    judged; owns(check) says whether a check name belongs to the caller."""
    known = json.loads(args.known.read_text())['known'] if args.known.exists() else {}
    mine = {k: v for k, v in known.items() if owns(k.split('|')[0])}
    seen = lambda key: tuple(key.split('|')[1:5]) in judged
    new = sorted(set(failures)-set(mine)); fixed = sorted(k for k in mine if k not in failures and seen(k))
    counts = {}
    for key in failures: counts[key.split('|')[0]] = counts.get(key.split('|')[0], 0)+1
    print('%s: failures by check: %s' % (label, dict(sorted(counts.items())) or 'none'))
    print('%s: %d listed as known, %d new, %d listed but passing' % (label, len(set(failures) & set(mine)), len(new), len(fixed)))
    for key in new: print('  NEW   %s: %s' % (key, failures[key]))
    for key in fixed: print('  FIXED %s (%s)' % (key, mine[key]['ref']))
    added = [k for k in new if args.add and (not args.match or re.search(args.match, k))]
    for key in added: known[key] = dict(ref=args.add, detail=failures[key], **({'finding': args.finding} if args.finding else {}))
    if args.prune:
        for key in fixed: del known[key]
    if added or (args.prune and fixed):
        args.known.write_text(json.dumps(dict(note=NOTE, known=dict(sorted(known.items()))), indent=1)+'\n')
    return 1 if set(new)-set(added) else 0

def relative(a, b):
    """|a-b| / max(1, |b|); 0 for equal values (including equal infinities)."""
    if a == b: return 0.0
    if not (math.isfinite(a) and math.isfinite(b)): return math.inf
    return abs(a-b)/max(1.0, abs(b))
