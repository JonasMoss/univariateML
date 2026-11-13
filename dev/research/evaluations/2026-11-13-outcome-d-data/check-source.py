#!/usr/bin/env python3
"""Verify fitter preservation against the job's baseline; no archived source trees."""
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parents[4]
BASE = '423aea9be43b9a325432bc170474557f665b33f8'
paths = subprocess.check_output(['git', 'diff', '--name-only', BASE], cwd=ROOT, text=True).splitlines()
adapters = {f'bindings/common/distributions/{name}/fit.hpp' for name in ('pois', 'binom', 'nbinom', 'geom', 'zeta', 'lgser')}
for base, names in {'pois': ('tpois', 'zip', 'zmpois', 'ztpois'), 'binom': ('tbinom', 'zib', 'zmbinom', 'ztbinom'), 'nbinom': ('zinb', 'zmnbinom', 'ztnbinom'), 'geom': ('zmgeom', 'ztgeom'), 'lgser': ('zmlogarithmic',), 'norm': ('tnorm',)}.items():
    adapters.update(f'bindings/common/distributions/{base}/{name}_fit.hpp' for name in names)
for name in paths:
    if name.startswith('bindings/'):
        assert name in adapters or name == 'bindings/common/modifiers.hpp', 'out-of-scope binding: ' + name
    if name.startswith('univariateML-cpp/'):
        assert name.startswith('univariateML-cpp/include/cppunivariate/distributions/'), 'out-of-scope core: ' + name
        assert name.split('/')[4] in ('pois', 'binom', 'nbinom', 'geom', 'zeta', 'lgser', 'norm'), name
count = 0
for name in paths:
    if not name.startswith('univariateML-cpp/include/'):
        continue
    old = subprocess.check_output(['git', 'show', BASE + ':' + name], cwd=ROOT, text=True)
    new = (ROOT / name).read_text()
    marker = '\nnamespace cppunivariate {\n'
    pos = new.rfind(marker)
    assert pos >= 0, name
    prefix = new[:pos].replace('#include "../../core/outcome.hpp"\n', '', 1)
    assert prefix.rstrip() == old.rstrip(), name + ': original fitter changed'
    mapping = new[pos:]
    assert 'default:' not in mapping, name + ': default in outcome switch'
    assert 'return {};' in mapping, name
    cases = re.findall(r'case ([\w:]+)::(\w+):', mapping)
    assert len(cases) == len(set(cases)), name + ': duplicate status'
    enum_name = cases[0][0].split('::')[-1]
    enum_text = new
    if enum_name in ('ZmbinomStatus', 'ZmnbinomStatus', 'ModifiedStatus'):
        enum_name = 'ModifiedStatus'
        enum_text = (ROOT / 'univariateML-cpp/include/cppunivariate/core/zero_modified_fit.hpp').read_text()
    elif enum_name == 'TruncationStatus':
        enum_text = (ROOT / 'univariateML-cpp/include/cppunivariate/core/truncation.hpp').read_text()
    body = re.search(r'enum class ' + enum_name + r'\s*\{(.*?)\}', enum_text, re.S).group(1)
    body = re.sub(r'//[^\n]*', '', body)
    names = {part.strip().split('=')[0].strip() for part in body.split(',') if part.strip()}
    assert {case[1] for case in cases} == names, name + ': non-exhaustive status mapping'
    count += 1
print(f'{count} core headers: original contents preserved; only an include and appended outcome mapping added.')
print('Outcome switches cover every enumerator, have distinct cases, no default, and a fallback return {}.')
print('Binding scope: six base adapters, fifteen private adapters, and the modifier seam only.')
print('Baseline:', BASE)
