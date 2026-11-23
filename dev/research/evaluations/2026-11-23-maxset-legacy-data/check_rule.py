"""Exercise the global consistency rule with valid and corrupt synthetic records."""
from copy import deepcopy
from pathlib import Path
import runpy
import sys

validation = Path(__file__).resolve().parents[3] / 'validation'
sys.path.insert(0, str(validation))
check = runpy.run_path(str(validation / 'fit-consistency.py'))['unique_false_needs_set']

estimate = {'mu': '0x0p+0'}
fit = dict(
    outcome={'kind': 'estimate', 'unique': False},
    unique=False,
    estimates=estimate,
    maximizer_set=dict(
        version=1, coordinates=['mu'], pieces=[
            dict(kind='points', points=[dict(estimate=[0]), dict(estimate=[1])])]),
    maximizers=[dict(estimates=estimate), dict(estimates={'mu': '0x1p+0'})],
)

assert not check(fit)
assert check(dict(fit, maximizer_set=None)) == 'missing typed maximizer set'
wrong_version = deepcopy(fit)
wrong_version['maximizer_set']['version'] = 2
assert check(wrong_version) == 'unknown version'
wrong_name = deepcopy(fit)
wrong_name['maximizer_set']['coordinates'] = ['scale']
assert check(wrong_name) == 'maximizer set coordinates differ from the estimate'
assert check(dict(fit, estimates={'mu': '0x1p+1'})) == 'estimate lies outside the decoded set'
assert check(dict(fit, maximizers=fit['maximizers'][::-1])) == 'first representative differs from selected estimate'
assert not check(dict(fit, unique=None, maximizer_set=None,
                      outcome={'kind': 'estimate', 'unique': None}))
print('Seven rule checks passed.')
