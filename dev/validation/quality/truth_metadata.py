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
