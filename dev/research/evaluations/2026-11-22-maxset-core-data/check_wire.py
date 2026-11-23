"""Negative coverage checks and exact JSON transport for the typed pilot sets."""
from copy import deepcopy
import json
import math
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[3] / 'validation'))
from flat_maxima import CASES, axis, box, points, typed_expected
from maximizer_sets import compare, normalize


def reject(actual, expected, reason, **controls):
    try:
        compare(actual, expected, {}, **controls)
    except ValueError as error:
        assert reason in str(error), error
    else:
        raise AssertionError(f'accepted broken {reason}')


binom = typed_expected('binom', CASES['binom']['zeros-flat-free'])
missing = deepcopy(binom)
missing['pieces'].pop()
reject(missing, binom, 'coverage')
missing = deepcopy(binom)
missing['pieces'][0]['axes'][1]['closed'][1] = False
reject(missing, binom, 'coverage')
redundant = deepcopy(binom)
redundant['pieces'].append(points((0, 0)))
compare(redundant, binom, {})
equivalent = deepcopy(binom)
equivalent['pieces'][0]['axes'][1]['upper'] = .5
equivalent['pieces'].append(box(axis(0, 0, (True, True), True), axis(.5, 1, (False, True))))
compare(equivalent, binom, {})

nhyper = typed_expected('nhyper', CASES['nhyper']['zeros-flat-free'])
missing = deepcopy(nhyper)
missing['pieces'][0]['axes'][1]['upper'] -= 1
reject(missing, nhyper, 'coverage')
extra = deepcopy(nhyper)
extra['pieces'][0]['order'].clear()
reject(extra, nhyper, 'coverage')

cauchy = typed_expected('cauchy', CASES['cauchy']['pair-flat-semicircle'])
two_arcs = deepcopy(cauchy)
right = deepcopy(two_arcs['pieces'][0])
two_arcs['pieces'][0]['axes'][0]['upper'] = 3
two_arcs['pieces'][0]['axes'][0]['closed'][1] = True
right['axes'][0]['lower'] = 3
two_arcs['pieces'].append(right)
compare(two_arcs, cauchy, {})
two_arcs['pieces'][0]['axes'][0]['closed'][1] = False
reject(two_arcs, cauchy, 'coverage')

roots = typed_expected('cauchy', CASES['cauchy']['pair-flat-reflected'])
enclosed = deepcopy(roots)
for value in enclosed['pieces'][0]['points']:
    x, scale = value['estimate']
    value['enclosure'] = dict(lower=[math.nextafter(x, -math.inf), scale],
                              upper=[math.nextafter(x, math.inf), scale])
compare(enclosed, roots, {}, location_scale=1)
missing = deepcopy(enclosed)
missing['pieces'][0]['points'].pop()
reject(missing, roots, 'missing enclosed branch', location_scale=1)
wide = deepcopy(enclosed)
wide['pieces'][0]['points'][0]['enclosure']['lower'][0] -= 1e-6
reject(wide, roots, 'coefficient tolerance', location_scale=1)

malformed = deepcopy(binom)
malformed['version'] = 2
reject(malformed, binom, 'unknown version')
malformed = deepcopy(binom)
malformed['pieces'][0]['kind'] = 'other'
reject(malformed, binom, 'unknown piece')
malformed = deepcopy(binom)
malformed['pieces'][0]['axes'][0]['lower'] = 'inf'
reject(malformed, binom, 'invalid bounds')
malformed['pieces'][0]['axes'][0]['lower'] = math.nan
reject(malformed, binom, 'invalid coefficient')
malformed = deepcopy(binom)
malformed['pieces'][0]['axes'].pop()
reject(malformed, binom, 'wrong axis dimension')
duplicate = deepcopy(enclosed)
duplicate['pieces'][0]['points'][1] = duplicate['pieces'][0]['points'][0]
reject(duplicate, roots, 'overlapping enclosures', location_scale=1)

wire = dict(version=1, coordinates=['x', 'y'], pieces=[
    box(axis('-inf', 'inf'), axis(0, 2**53 - 1, (True, True), True)),
    points((math.ulp(0.), 1.), (math.ulp(0.), math.nextafter(1., math.inf)))])
assert json.loads(json.dumps(wire, allow_nan=False)) == wire
assert normalize(json.loads(json.dumps(wire)), ['x', 'y']) == normalize(wire, ['x', 'y'])
print('Coverage negatives, equivalent decompositions, and binary64 JSON transport passed.')
