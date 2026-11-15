# Rebuild the fit-snapshot case list and compare input hashes with stored snapshots (no fitting).
# usage: python3 hashcheck.py DIR   (DIR holds native/ and routes-native/)
import importlib.util, gzip, json, hashlib, sys
from pathlib import Path
root = Path(__file__).resolve().parents[4]
spec = importlib.util.spec_from_file_location('fs', root/'dev/validation/fit-snapshot.py')
fs = importlib.util.module_from_spec(spec); spec.loader.exec_module(fs)
snap = Path(sys.argv[1])  # the stored post-5b snapshots, e.g. validate/build/snapshots
archive = {}
for c in json.loads(gzip.decompress(fs.ARCHIVE.read_bytes())): archive.setdefault(c['family'], []).append(c)
for mode in ('native', 'routes-native'):
    fs.ROUTE_MODE = mode.startswith('routes')
    sel = sorted(fs.ROUTES) if fs.ROUTE_MODE else fs.families()
    same = diff = missing = new = 0; new_cls = set()
    for ident in sel:
        part = snap/mode/(ident+'.jsonl')
        old = {}
        if part.exists():
            for l in part.read_text().split('\n')[1:]:
                if l.strip():
                    r = json.loads(l); old[(r['source'], r['case'], json.dumps(r['mask']), json.dumps(r['held'], sort_keys=True))] = r['input_sha256']
        meta, _ = fs.read(ident); names = [p['name'] for p in meta['params']]
        seen = set()
        for case in fs.cases(ident, archive):
            h = hashlib.sha256(fs.line(case, names).encode()).hexdigest()[:16]
            k = (case['source'], case['case'], json.dumps(case['mask']), json.dumps(case['held'], sort_keys=True))
            if k in old:
                seen.add(k)
                if old[k] == h: same += 1
                else: diff += 1; print('DIFF', ident, k)
            else: new += 1; new_cls.add(case['case'] if case['source'] == 'class' else case['source'])
        missing += len(set(old) - seen)
    print(mode, 'identical', same, 'different', diff, 'old-not-rebuilt', missing, 'new', new, sorted(new_cls))
