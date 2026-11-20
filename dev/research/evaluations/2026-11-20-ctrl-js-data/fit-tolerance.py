#!/usr/bin/env python3
"""Measure the golden fits that pass kFitTol explicitly in C++ family tests.

Run from the repository root under the single-slot wrapper. Arguments are the
output directory and an optional --no-long-double.
"""
import argparse
import csv
import json
import os
import re
from pathlib import Path
import subprocess

import mpmath as mp

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('output', type=Path)
parser.add_argument('--no-long-double', action='store_true')
args = parser.parse_args()
args.output.mkdir(parents=True, exist_ok=True)
dossiers = Path('univariateML-cpp/include/cppunivariate/distributions')

# Same core calls and result-field names as the corresponding golden tests.
# Binomial supplies a free size and the two count families use integer spans.
probes = {
    'binom': ('binom_mle(ix, NAN, tol, 100)', {'size': 'size', 'prob': 'prob'}),
    'cauchy': ('fit_cauchy(x, tol, 100)', {'location': 'mu', 'scale': 'scale'}),
    'fatigue': ('fatigue_mle(x, tol, 100)', {'alpha': 'alpha', 'beta': 'beta'}),
    'gompertz': ('gompertz_mle(x, tol, 100)', {'a': 'a', 'b': 'b'}),
    'gumbel': ('fit_gumbel_newton(x, tol, 100)', {'mu': 'mu', 'sigma': 'sigma'}),
    'invgamma': ('invgamma_mle(x, tol, 100)', {'alpha': 'shape', 'beta': 'scale'}),
    'invparalogis': ('invparalogis_mle(x, tol, 100)', {'shape': 'shape', 'scale': 'scale'}),
    'invpareto': ('invpareto_mle(x, tol, 100)', {'shape': 'shape', 'scale': 'scale'}),
    'invweibull': ('invweibull_mle(x, tol, 100)', {'shape': 'shape', 'scale': 'scale'}),
    'kumar': ('kumar_mle(x, tol, 100)', {'a': 'a', 'b': 'b'}),
    'llogis': ('loglogistic_mle(x, tol, 100)', {'shape': 'shape', 'scale': 'scale'}),
    'logis': ('logistic_mle(x, tol, 100)', {'location': 'mu', 'scale': 'scale'}),
    'lomax': ('lomax_mle(x, tol, 100)', {'lambda': 'lambda', 'kappa': 'kappa'}),
    'naka': ('naka_mle(x, tol, 100)', {'shape': 'shape', 'scale': 'scale'}),
    'nbinom': ('nbinom_mle(ix, tol, 100)', {'size': 'size', 'prob': 'prob'}),
    'paralogis': ('paralogis_mle(x, tol, 100)', {'shape': 'shape', 'scale': 'scale'}),
    'weibull': ('weibull_mle(x, tol, 100)', {'shape': 'shape', 'scale': 'scale'}),
}

source = args.output / 'fit-tolerance.cpp'
binary = args.output / 'fit-tolerance'
includes = [f'#include "cppunivariate/distributions/{family}/mle.hpp"' for family in probes]
lines = [
    '#include "test_support.hpp"', '#include <iostream>', '#include <iomanip>',
    *includes, 'int main() {',
    '  std::cout << std::setprecision(17);',
    '  for (const double tol : {1.220703e-4, mltest::kFitTol}) {',
]
references = {}


def add_probe(label, call, fields, fixture, estimates, kind):
    references[label] = (estimates, kind)
    lines.extend(['  {', f'    const auto g = mltest::load_golden("{fixture.resolve()}");'])
    if 'ix,' in call:
        lines.extend(['    const std::vector<int> v(g.data.begin(), g.data.end());',
                      '    const cppunivariate::ispan ix{v.data(), v.size()};'])
    else:
        lines.append('    const auto x = mltest::as_span(g.data);')
    lines.append(f'    const auto fit = cppunivariate::{call};')
    for name, field in fields.items():
        lines.append(f"    std::cout << \"{label},{name},\" << tol << ',' << static_cast<int>(fit.status)"
                     f" << ',' << fit.{field} << ',' << mltest::kFitReferenceTol << '\\n';")
    lines.append('  }')


for family, (call, fields) in probes.items():
    fixture = dossiers / family / 'golden.json'
    golden = json.loads(fixture.read_text())
    reference = golden.get('reference', {}).get('estimates')
    add_probe(family, call, fields, fixture,
              reference or golden['estimates'], 'reference' if reference else 'golden')


def outlier_reference(family, fixture):
    # Solve in location/log-scale coordinates. Log-logistic on the original
    # sample becomes logistic on log(x), with the observation-only Jacobian.
    with mp.workdps(60):
        x = [mp.mpf(v) for v in fixture['data']]
        first, second = map(mp.mpf, (fixture['estimates']['first'], fixture['estimates']['second']))
        if family == 'llogis':
            x = list(map(mp.log, x))
            first, second = -mp.log(second), 1 / first

        def log_kernel(z):
            if family == 'cauchy':
                return -mp.log1p(z*z)
            if family == 'gumbel':
                return -z-mp.exp(-z)
            return -z-2*mp.log1p(mp.exp(-z))

        def likelihood(location, log_scale):
            scale = mp.exp(log_scale)
            return mp.fsum(log_kernel((v-location)/scale) for v in x) - len(x)*log_scale

        def score(location, log_scale):
            return tuple(mp.diff(likelihood, (location, log_scale), order)
                         for order in ((1, 0), (0, 1)))

        location, log_scale = mp.findroot(score, (first, mp.log(second)))
        scale = mp.exp(log_scale)
        return (1/scale, mp.exp(location)) if family == 'llogis' else (location, scale)


for family in ('cauchy', 'gumbel', 'llogis', 'logis'):
    fixture = dossiers / family / 'fixture-outlier.json'
    call, fields = probes[family]
    values = outlier_reference(family, json.loads(fixture.read_text()))
    add_probe(f'{family}/outlier', call, fields, fixture, dict(zip(fields, values)), 'score-root')

def sample(name, observations, estimates):
    fixture = args.output / (name + '.json')
    fixture.write_text(json.dumps(dict(data=observations, estimates=estimates, logLik=0)))
    return fixture


values = [-2.2425190273413, 0.723253413022922, -2.56072856385821]
old_reference = {'first': -2.369470147, 'second': 0.272740500}
fixture = sample('cauchy-three', values, old_reference)
call, fields = probes['cauchy']
values = outlier_reference('cauchy', dict(data=values, estimates=old_reference))
add_probe('cauchy/three', call, fields, fixture, dict(zip(fields, values)), 'score-root')

test = (dossiers / 'logis/test.cpp').read_text()
region = test.split('const auto x=read({', 1)[1].split('});', 1)[0]
values = [float.fromhex(word) for word in re.findall(r'"([^"]+)"', region)]
location = {'location': '100000000.116207034405182'}
fixture = sample('logis-held-scale', values, location)
add_probe('logis/held-scale', 'logistic_mle(x, tol, 100, NAN, 1.0081430589945095)',
          {'location': 'mu'}, fixture, location, 'test-reference')

# Binomial probability boundaries are attained exactly at p=0 or p=1.
for label, count, held, size in (
        ('ones-free-size', 5, 'NAN', 5), ('ones-held-size', 5, '5', 5),
        ('zeros-free-size', 0, 'NAN', 0), ('zeros-held-zero-size', 0, '0', 0),
        ('zeros-held-size', 0, '5', 5)):
    expected = {'size': size, 'prob': int(count > 0)}
    fixture = sample('binom-' + label, [count] * 3, expected)
    add_probe('binom/' + label, f'binom_mle(ix, {held}, tol, 100)',
              probes['binom'][1], fixture, expected, 'exact')

lines += ['  }', '}']
source.write_text('\n'.join(lines) + '\n')

command = [
    os.environ.get('CXX', 'c++'), '-std=c++17', '-O2', '-DNDEBUG', '-DDOCTEST_CONFIG_DISABLE',
    '-IunivariateML-cpp/include', '-IunivariateML-cpp/tests',
    f'-DCPPUNIV_DISTRIBUTIONS_DIR="{dossiers.resolve()}"',
    '-DCPPUNIV_MODIFIER_FIXTURES_DIR=""',
]
if not Path('/usr/include/boost/math/distributions/gamma.hpp').is_file():
    boost = subprocess.check_output(
        ['Rscript', '-e', 'cat(system.file("include", package="BH"))'], text=True)
    command.extend(['-I', boost])
if args.no_long_double:
    command.append('-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS')
subprocess.run([*command, str(source), '-o', str(binary)], check=True)
measurements = subprocess.check_output([binary], text=True)
failures = []
with (args.output / 'fit-tolerance.csv').open('w') as out:
    writer = csv.writer(out)
    writer.writerow(['family', 'coefficient', 'solver_tol', 'status', 'fit', 'reference',
                     'reference_source', 'relative_error', 'contract_comparison_tol'])
    for family, name, tol, status, fit, threshold in csv.reader(measurements.splitlines()):
        values, kind = references[family]
        reference = float(values[name])
        error = abs(float(fit) - reference) / abs(reference) if reference else abs(float(fit))
        writer.writerow([family, name, tol, status, fit, reference, kind, error, threshold])
        if float(tol) < 1e-5 and (int(status) != 0 or not error <= float(threshold)):
            failures.append(f'{family}/{name}, status {status}, error {error:.3g}')
if failures:
    raise SystemExit('Coefficient contract checks failed: ' + '; '.join(failures))
