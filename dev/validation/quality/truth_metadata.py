"""Migrate truth metadata without evaluating or reserializing numerical rows.

Run through the CPU slot lock with PYTHONPATH=dev/validation:
python3 -m quality.truth_metadata
"""
import hashlib
import json
import re
from quality.truth import DISTS, ROOT, input_sha256


def numerical_bytes(text):
    # Everything except the fingerprint and elapsed times must remain verbatim.
    text = re.sub(r',\n      "seconds": [^\n]+', '', text)
    return re.sub(r'  "(?:density|input)_sha256": "[0-9a-f]+"', '', text).encode()


def _number(v):
    if isinstance(v, bool): return None
    if isinstance(v, (int, float)): return float(v)
    if isinstance(v, str):
        try: return float(v)
        except ValueError: return None
    return None


def payload_close(a, b, rtol=1e-9, atol=1e-12, path='$'):
    """Differences between two parsed tables, ignoring metadata; [] if equal.

    Structure and non-numeric strings must match exactly; numbers (including
    numeric strings such as normalization masses) must agree within
    rtol*max(|a|,|b|)+atol. Not byte-level: see the fingerprint policy.
    """
    if isinstance(a, dict) and isinstance(b, dict):
        out = []
        for k in sorted(set(a) | set(b)):
            if k in ('input_sha256', 'density_sha256', 'seconds'): continue
            if k not in a or k not in b: out.append(f'{path}.{k}: key missing'); continue
            out += payload_close(a[k], b[k], rtol, atol, f'{path}.{k}')
        return out
    if isinstance(a, list) and isinstance(b, list):
        if len(a) != len(b): return [f'{path}: length {len(a)} vs {len(b)}']
        out = []
        for i, (x, y) in enumerate(zip(a, b)): out += payload_close(x, y, rtol, atol, f'{path}[{i}]')
        return out
    x, y = _number(a), _number(b)
    if x is not None and y is not None:
        if x == y or abs(x-y) <= rtol*max(abs(x), abs(y))+atol: return []
        return [f'{path}: {a} vs {b}']
    return [] if a == b else [f'{path}: {a!r} vs {b!r}']


def main():
    before = hashlib.sha256()
    after = hashlib.sha256()
    count = 0
    for folder in sorted(DISTS.iterdir()):
        path = folder/'truth.json'
        if not path.exists():
            continue
        old = path.read_text()
        new = re.sub(r',\n      "seconds": [^\n]+', '', old)
        new = re.sub(r'"(?:density|input)_sha256": "[0-9a-f]+"',
                     '"input_sha256": "'+input_sha256(folder)+'"', new)
        assert numerical_bytes(old) == numerical_bytes(new), folder.name
        # Parse too: a malformed or unexpectedly formatted file must stop.
        left, right = json.loads(old), json.loads(new)
        left.pop('density_sha256', None)
        left.pop('input_sha256', None)
        right.pop('input_sha256')
        for row in left['normalization']:
            row.pop('seconds', None)
        assert left == right, folder.name
        before.update(folder.name.encode()+b'\0'+numerical_bytes(old))
        after.update(folder.name.encode()+b'\0'+numerical_bytes(new))
        path.write_text(new)
        count += 1
    # Historical generation statuses retain their counts/results, not timings.
    audit = ROOT/'dev/research/evaluations/2026-10-29-dpqr-truth'
    for path in audit.glob('*.json'):
        data = json.loads(path.read_text())
        if isinstance(data, dict) and data.get('status') in ('generated', 'gap') and 'seconds' in data:
            data.pop('seconds')
            path.write_text(json.dumps(data, indent=2)+'\n')
    print(f'{count} tables: numerical payload bytes identical')
    print('before:', before.hexdigest())
    print('after: ', after.hexdigest())


if __name__ == '__main__':
    main()
