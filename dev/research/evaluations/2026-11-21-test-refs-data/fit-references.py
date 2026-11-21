#!/usr/bin/env python3
"""From the repository root, run under /tmp/umvl-cpu/run.

Emit a CSV for each precision mode alongside this script. The C++ probe
supplies the exact inputs and fit measurements. Only mpmath computes the
references, by solving the likelihood scores as in the ctrl-js tolerance study.
"""
import csv
import os
from pathlib import Path
import re
import subprocess

import mpmath as mp


DOSSIERS = Path('univariateML-cpp/include/cppunivariate/distributions')
SCRATCH = Path('build/scratch/test-refs')
OUTPUT = Path(__file__).resolve().parent
CASES = {
    'cauchy/outlier': ('fit_cauchy', ('location', 'scale'), ('r.mu', 'r.scale'), (0, 1)),
    'cauchy/three': ('fit_cauchy', ('location', 'scale'), ('r.mu', 'r.scale'), (-2.4, 0.3)),
    'gumbel/outlier': ('fit_gumbel_newton', ('location', 'scale'), ('r.mu', 'r.sigma'), (0, 5)),
    'llogis/outlier': ('loglogistic_mle', ('shape', 'reciprocal scale'),
                       ('r.shape', '1.0 / r.scale'), (0, 0.5)),
    'logis/outlier': ('logistic_mle', ('location', 'scale'), ('r.mu', 'r.scale'), (0, 5)),
}


def solve(family, data, start, digits):
    with mp.workdps(digits):
        x = [mp.mpf(value) for value in data]
        if family == 'llogis':
            x = list(map(mp.log, x))

        def score(location, log_scale):
            z = [(v - location) * mp.exp(-log_scale) for v in x]
            if family == 'cauchy':
                weights = [2 * v / (1 + v*v) for v in z]
            elif family == 'gumbel':
                weights = [1 - mp.exp(-v) for v in z]
            else:
                weights = [mp.tanh(v/2) for v in z]
            return mp.fsum(weights), mp.fsum(v*w for v, w in zip(z, weights)) - len(z)

        location, log_scale = mp.findroot(score, (start[0], mp.log(start[1])))
        assert max(map(abs, score(location, log_scale))) < mp.mpf(10)**(10-digits)
        scale = mp.exp(log_scale)
        values = (1/scale, mp.exp(-location)) if family == 'llogis' else (location, scale)
        return [mp.nstr(value, digits) for value in values]


def probe():
    lines = ['#include "test_support.hpp"', '#include <iomanip>', '#include <iostream>']
    for family in dict.fromkeys(case.split('/')[0] for case in CASES):
        lines.append(f'#include "cppunivariate/distributions/{family}/mle.hpp"')
    lines.append('int main() {')
    for case, (call, _, fields, _) in CASES.items():
        family, fixture = case.split('/')
        lines.append('  {')
        if fixture == 'three':
            test = (DOSSIERS / family / 'test.cpp').read_text()
            region = test.split('TEST_CASE("cauchy: three observations', 1)[1]
            declaration = re.search(r'const std::vector<double> x\{[^}]+\};', region)
            lines.append('    ' + declaration[0])
        else:
            path = (DOSSIERS / family / 'fixture-outlier.json').resolve()
            lines.append(f'    const auto g = mltest::load_golden("{path}");')
            lines.append('    const auto& x = g.data;')
        lines.extend([
            f'    const auto r = cppunivariate::{call}(mltest::as_span(x),'
            ' mltest::kFitTol, mltest::kFitMaxit);',
            f'    std::cout << "{case}," << static_cast<int>(r.status) << std::hexfloat'
            f' << \',\' << {fields[0]} << \',\' << ({fields[1]});',
            "    for (double v : x) std::cout << ',' << v;",
            "    std::cout << '\\n';",
            '  }',
        ])
    return '\n'.join(lines + ['}', ''])


def main():
    SCRATCH.mkdir(parents=True, exist_ok=True)
    source = SCRATCH / 'probe.cpp'
    binary = SCRATCH / 'probe'
    source.write_text(probe())
    command = [
        os.environ.get('CXX', 'c++'), '-std=c++17', '-O2', '-DNDEBUG',
        '-DDOCTEST_CONFIG_DISABLE', '-IunivariateML-cpp/include', '-IunivariateML-cpp/tests',
        f'-DCPPUNIV_DISTRIBUTIONS_DIR="{DOSSIERS.resolve()}"',
        '-DCPPUNIV_MODIFIER_FIXTURES_DIR=""',
    ]
    if not Path('/usr/include/boost/math/distributions/gamma.hpp').is_file():
        boost = subprocess.check_output(
            ['Rscript', '-e', 'cat(system.file("include", package="BH"))'], text=True)
        command.extend(['-I', boost])

    native_inputs = {}
    for mode, flags in [('native', []), ('double', ['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'])]:
        subprocess.run([*command, *flags, str(source), '-o', str(binary)], check=True)
        output = subprocess.check_output([binary], text=True)
        with (OUTPUT / f'{mode}.csv').open('w', newline='') as out:
            writer = csv.writer(out, lineterminator='\n')
            writer.writerow(['case', 'coefficient', 'n', 'status', 'score_root_100_digits',
                             'binary64', 'binary64_hex', 'fit', 'relative_error'])
            for case, status, first, second, *inputs in csv.reader(output.splitlines()):
                assert native_inputs.setdefault(case, inputs) == inputs
                data = list(map(float.fromhex, inputs))
                _, names, _, start = CASES[case]
                family = case.split('/')[0]
                roots = solve(family, data, start, 100)
                rounded = list(map(float, roots))
                assert rounded == list(map(float, solve(family, data, start, 70)))
                for name, root, ref, fit_hex in zip(names, roots, rounded, (first, second)):
                    fit = float.fromhex(fit_hex)
                    error = abs(fit - ref) / abs(ref)
                    writer.writerow([case, name, len(data), status, root, format(ref, '.17g'),
                                     ref.hex(), format(fit, '.17g'), error])


if __name__ == '__main__':
    main()
