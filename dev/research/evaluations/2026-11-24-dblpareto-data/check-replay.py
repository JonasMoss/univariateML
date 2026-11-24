"""Compare a fresh mode to its stored baseline and independently replay face payloads.

Run with two arguments, the baseline .jsonl.gz and the fresh snapshot directory.
The output records every existing result change, the new and missing case counts,
and an mpmath density sum for every selected double Pareto face.
"""
import argparse
import gzip
import json
from collections import Counter
from pathlib import Path
import sys

import mpmath as mp

ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT / 'dev/validation'))
from strict_common import snapshot_module


def face_loglik(row, data):
    res = row['result']
    payload = res['limit_estimates']
    number = lambda name: float.fromhex(payload[name])
    b, a, r = (number(k) for k in ('scale', 'left_rate', 'right_rate'))
    offset = number('min') if 'min' in payload else 0.0
    with mp.workdps(100):
        b, a, r = map(mp.mpf, (b, a, r))
        normalizer = a*r/(b*(a+r))
        terms = []
        for x in data:
            y = mp.mpf(float(x - offset))
            exponent = a-1 if y <= b else -r-1
            terms.append(mp.log(normalizer * (y/b)**exponent))
        reference = mp.fsum(terms)
        reported = float.fromhex(res['limit_loglik'])
        relative = abs(reference - reported) / max(1, abs(reference))
        assert relative < mp.mpf('1e-10'), (row['family'], row['case'], str(relative))
        return {'mpmath_loglik': float(reference), 'relative_error': float(relative)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('baseline')
    parser.add_argument('fresh')
    args = parser.parse_args()
    fs = snapshot_module()
    before, after = fs.load(args.baseline), fs.load(args.fresh)
    archive = {}
    for row in json.loads(gzip.decompress(fs.ARCHIVE.read_bytes())):
        archive.setdefault(row['family'], []).append(row)
    faces = {'dblpareto', 'unavailable:shifted-dblpareto'}
    inputs = {
        family: {(c['source'], c['case']): c['data'] for c in fs.cases(family, archive)}
        for family in ('gb2', 'fpareto')
    }
    counts = Counter()
    for key in sorted(before.keys() | after.keys()):
        old, new = before.get(key), after.get(key)
        if old is None:
            counts['added:' + key[0]] += 1
            continue
        if new is None:
            counts['missing:' + key[0]] += 1
            continue
        if old['input_sha256'] != new['input_sha256']:
            counts['input-changed:' + key[0]] += 1
            print(json.dumps(dict(key=key, input_before=old['input_sha256'],
                                  input_after=new['input_sha256'])))
            continue
        a, b = old['result'], new['result']
        fields = {k: [a.get(k), b.get(k)] for k in sorted(a.keys() | b.keys())
                  if a.get(k) != b.get(k)}
        selected = b.get('model') in faces and b.get('kind') == 'limit'
        if not fields and not selected:
            continue
        out = dict(key=key, changed=fields)
        if fields:
            counts['changed:' + key[0]] += 1
        if selected:
            assert a.get('kind') == b['kind']
            assert a.get('core_status') == b.get('core_status')
            assert a.get('outcome') == b.get('outcome')
            assert set(fields) <= {'model', 'limit_estimates', 'limit_loglik', 'logLik',
                                   'message', 'candidates'}, fields
            out.update(face_loglik(new, inputs[key[0]][key[1:]]))
            counts['face:' + key[0]] += 1
        print(json.dumps(out, sort_keys=True))
    print(json.dumps(dict(baseline=len(before), fresh=len(after),
                          counts=dict(sorted(counts.items())))))


if __name__ == '__main__':
    main()
