#!/usr/bin/env python3
"""Assert the core diff only adds outcome overloads and their include."""
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parents[4]
FAMILIES = 'beta bnbinom cmb dlaplace dweibull fatigue frechet genbeta gompertz hcauchy huber invexp invparalogis invweibull llogis logitnorm naka paralogis pareto3 power sgomp sn st trgamma weibull'.split()
for family in FAMILIES:
    path = Path('univariateML-cpp/include/cppunivariate/distributions') / family / 'mle.hpp'
    original = subprocess.check_output(['git', 'show', '1e0f5185432e858b39ceffa18f043ce4d54993b1:' + str(path)], cwd=ROOT, text=True)
    current = (ROOT / path).read_text().replace('#include "../../core/outcome.hpp"\n', '')
    while match := re.search(r'inline FitOutcome outcome\([^)]*\)\s*\{', current):
        depth = 1
        end = match.end()
        while depth:
            if current[end] == '{': depth += 1
            if current[end] == '}': depth -= 1
            end += 1
        current = current[:match.start()] + current[end:]
    assert re.sub(r'\s+', '', current) == re.sub(r'\s+', '', original), family
print('25/25 core headers: unchanged after removing added outcome overloads and include (whitespace ignored).')
