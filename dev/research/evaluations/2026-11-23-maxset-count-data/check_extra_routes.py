"""Replay count routes that have no alias in the frozen route snapshots.

Run from the worktree with /tmp/umvl-cpu/run python3 <this file>.
The normal snapshot probe compiles and calls the modifier dispatcher.
"""
import importlib
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT / 'dev/validation'))
snapshot = importlib.import_module('fit-snapshot')
from flat_maxima import CASES, typed_expected
import maximizer_sets
from strict_common import number


ROUTES = [
    ('zigeom', 'zinb', 'geom', 'inflated', ('prob', 'pi'), 'zigeom'),
    ('geom-window', 'tnorm', 'geom', 'none', ('prob', 'a', 'b'), 'geom'),
    ('nbinom-window', 'tnorm', 'nbinom', 'none', ('size', 'prob', 'a', 'b'), 'nbinom'),
]


def check(result, family, case, owner):
    assert result['kind'] == 'ok', result
    assert result['unique'] is case.unique, result
    assert result['outcome']['unique'] is case.unique, result
    assert number(result['logLik']) == 0, result
    expected = typed_expected(family, case)
    names = expected['coordinates']
    held = {k: v for k, v in case.held.items() if k in names}
    selected = tuple(number(result['estimates'][name]) for name in names)
    wire = result['maximizer_set']
    if case.unique:
        assert wire is None, wire
        actual = dict(version=1, coordinates=names, pieces=[
            dict(kind='points', points=[dict(estimate=list(selected))])])
        maximizer_sets.compare(actual, expected, held)
        return
    decoded = maximizer_sets.compare(wire, expected, held)
    assert maximizer_sets.member(selected, decoded), selected
    assert result['maximizers'][0]['estimates'] == result['estimates']
    for row in result['maximizers']:
        assert row['model'] == owner, row
        assert number(row['loglik_lower']) == number(row['loglik_upper']) == 0, row
        assert maximizer_sets.member(tuple(number(row['estimates'][k]) for k in names), decoded), row


def main():
    snapshot.ROUTE_MODE = True
    boost = snapshot.boost_include()
    for double in (False, True):
        mode = 'double' if double else 'native'
        for family, probe_alias, base, zero, coordinates, owner in ROUTES:
            # The existing probe's normal-window branch passes inclusive bounds.
            work = ROOT / 'build' / 'scratch' / 'extra-count-routes' / mode / family
            work.mkdir(parents=True, exist_ok=True)
            snapshot.ROUTES = {probe_alias: dict(base=base, zero=zero)}
            snapshot.read = lambda _ident: (dict(params=[dict(name=k) for k in coordinates]), None)
            probe = snapshot.Probe(snapshot.build(probe_alias, work, double, boost))
            try:
                for label, case in CASES[family].items():
                    mask = sum(1 << i for i, key in enumerate(coordinates) if key in case.held)
                    request = dict(mask=mask, held=case.held, data=case.data)
                    result = probe.ask(snapshot.line(request, coordinates), 20)
                    check(result, family, case, owner)
                    print(mode, family, label, 'PASS', flush=True)
            finally:
                probe.close()


if __name__ == '__main__':
    main()
