"""Check before/after coverage, cold/warm agreement and the exception exit."""
import collections
import gzip
import json
from pathlib import Path
import runpy
import sys

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
VALIDATION = ROOT/'dev/validation'
sys.path.insert(0, str(VALIDATION))


def read(name):
    rows = [json.loads(line) for line in (HERE/(name+'.jsonl')).read_text().splitlines()]
    return {(r['mode'], r['family'], r['source'], r['case']): r for r in rows}


def verify_records():
    before, after, warm = map(read, ('before', 'after', 'warm'))
    assert before.keys() == after.keys(), 'selected estimate coverage changed'
    assert after == warm, 'warm and fresh verdicts differ'
    expected = {
        'triang': ('ZeroDivisionError:', 36, 'checked'),
        'binom+zero=modified': ('KeyError:', 22, 'checked'),
        'nhyper': ('ValueError: Exceeds the limit', 4, 'checked'),
        'cmp': ('AssertionError:', 2, 'open'),
        'dgamma': ('AssertionError:', 2, 'open'),
    }
    for family, (prefix, count, status) in expected.items():
        affected = [key for key, r in before.items() if key[1] == family and r.get('reason', '').startswith(prefix)]
        assert len(affected) == count, (family, len(affected), count)
        assert {after[key]['status'] for key in affected} == {status}, family
        for key in affected:
            if status == 'open':
                assert 'series guard' in after[key]['reason'], (key, after[key])
    assert collections.Counter(r['status'] for r in after.values()) == {'checked': 216, 'open': 36}
    print('252 matching case keys, 216 comparisons passed, 36 open, cold and warm identical')


def verify_endpoint_classes():
    from refine_profiles import triang_endpoint

    classes = {'near-constant', 'offset', 'subnormal'}
    for mode in ('native', 'double'):
        with gzip.open(HERE/(mode+'.jsonl.gz'), 'rt') as source:
            rows = [r for r in map(json.loads, source) if r['family'] == 'triang' and r['case'] in classes]
        assert len(rows) == len(classes)
        for row in rows:
            result = row['result']
            for candidate in [result, *result['candidates']]:
                fitted = {k: float.fromhex(v) for k, v in candidate['estimates'].items()}
                profile = triang_endpoint(row['data'], fitted, set())
                for name in ('a', 'b'):
                    assert float(profile['estimates'][name]) == fitted[name], (row['case'], name, profile)
    print('Triangular near-constant, offset and subnormal class endpoints agree in both modes')


def verify_reference_scope():
    from copy import deepcopy

    m = runpy.run_path(str(VALIDATION/'fit-refine.py'))
    rows = m['load'](str(HERE/'routes-native.jsonl.gz'))
    key = ('binom+zero=modified', 'golden', 'golden')
    alternate = deepcopy(rows[key])
    alternate['result']['estimates']['size'] = float(9).hex()
    pairs = {'native': {key: rows[key]}, 'double': {key: alternate}}
    native, double = m['selected_cases']([key], pairs, {})
    assert native[1][1] == double[1][1]  # Same data, provenance and holds.
    assert native[1][0] != double[1][0]  # Distinct reference keys for sizes 8 and 9.
    probabilities = [m['refine'](*case[2], 120)[0]['estimates']['prob'] for case in (native, double)]
    assert probabilities[0] != probabilities[1], probabilities
    print('Two sizes on the same modifier sample produce distinct reference keys and probabilities')


def verify_exception(native):
    globals_ = runpy.run_path(str(VALIDATION/'fit-refine.py'))['main'].__globals__
    original = globals_['reference_engine']

    def broken(*_):
        raise ArithmeticError('injected reference exception')

    globals_['reference_engine'] = broken
    try:
        result, _ = globals_['refine']('binom', globals_['law_meta']('binom'),
                                       [1, 2], {'size': 3, 'prob': 0.5}, {'size'}, 120)
    finally:
        globals_['reference_engine'] = original
    assert result == {'exception': 'ArithmeticError: injected reference exception'}, result
    scratch_cache = ROOT/'build/scratch/refine-injected.jsonl.gz'
    with gzip.open(globals_['CACHE'], 'rt') as source, gzip.open(scratch_cache, 'wt') as target:
        injected = 0
        for line in source:
            row = json.loads(line)
            if row['family'] == 'triang' and row['case'] == 'golden':
                row.update(result)
                injected += 1
            target.write(json.dumps(row)+'\n')
        assert injected == 1
    globals_['CACHE'] = scratch_cache
    sys.argv = ['fit-refine.py', native, '--family', 'triang', '--cache-only']
    try:
        globals_['main']()
    except SystemExit as e:
        assert e.code == 1, e.code
    else:
        raise AssertionError('checker returned without an exception exit code')
    print('Injected reference exception was recorded and warm strict check exited 1')


if __name__ == '__main__':
    verify_records()
    verify_endpoint_classes()
    verify_reference_scope()
    verify_exception(sys.argv[1])
