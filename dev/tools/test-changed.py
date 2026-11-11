#!/usr/bin/env python3
"""Conservative changed-family runner. Git is read-only; --dry-run explains selection."""
import argparse
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
DOSSIERS = Path('univariateML-cpp/include/cppunivariate/distributions')


def git(*args):
    return subprocess.check_output(['git', *args], cwd=ROOT).decode()


def changed_paths(base):
    ancestor = git('merge-base', base, 'HEAD').strip()
    paths = set()
    for args in [('diff', '--no-renames', '--name-only', '-z', ancestor, 'HEAD'),
                 ('diff', '--no-renames', '--name-only', '-z', '--cached'),
                 ('diff', '--no-renames', '--name-only', '-z'),
                 ('ls-files', '--others', '--exclude-standard', '-z')]:
        paths.update(filter(None, git(*args).split('\0')))
    return ancestor, sorted(paths)


def select(paths):
    families = {p.name for p in (ROOT / DOSSIERS).iterdir() if (p / 'mle.hpp').exists()}
    reasons = {}
    shared = []
    for path in paths:
        match = re.match(r'(?:univariateML-cpp/include/cppunivariate/distributions|bindings/(?:common|r)/distributions)/([^/]+)/', path)
        test = re.match(r'bindings/r/tests/testthat/test-dist-([^/]+)\.R$', path)
        family = (match or test).group(1) if match or test else None
        if family in families:
            reasons.setdefault(family, []).append(path)
        elif path.startswith(('bindings/r/man/', 'bindings/r/vignettes/')):
            continue
        elif (path.startswith(('univariateML-cpp/', 'bindings/', 'dev/tools/',
                               'dev/validation/')) or Path(path).name in ('Justfile', 'CMakeLists.txt')):
            # Plain documentation outside dossiers cannot affect a test run.
            if Path(path).suffix not in ('.md', '.Rmd', '.Rd', '.tex', '.bib', '.pdf'):
                shared.append(path)
    # Resolve literal includes from implementation headers, including helpers outside
    # family folders. Walk reverse edges at file level before attributing families.
    headers = list((ROOT / 'univariateML-cpp/include').rglob('*.hpp'))
    headers += list((ROOT / 'bindings/common').rglob('*.h'))
    headers += list((ROOT / 'bindings/common').rglob('*.hpp'))
    headers += list((ROOT / 'univariateML-cpp/tests').glob('*.hpp'))
    headers += list((ROOT / 'univariateML-cpp/tests').glob('*.h'))
    headers += list((ROOT / 'bindings/r/scripts').glob('*.h'))
    headers = sorted(headers)
    reverse = {}
    changed_headers = {(ROOT / p).resolve() for p in paths if Path(p).suffix in ('.h', '.hpp')}
    for header in headers:
        for name in re.findall(r'^\s*#\s*include\s*["<]([^">]+)[">]', header.read_text(), re.M):
            for candidate in (header.parent / name, ROOT / 'univariateML-cpp/include' / name,
                              ROOT / 'univariateML-cpp/tests' / name,
                              ROOT / 'bindings' / name, ROOT / 'bindings/r/scripts' / name):
                target = candidate.resolve()
                if target.is_file() or target in changed_headers:
                    reverse.setdefault(target, set()).add(header.resolve())
                    break
    queue = [p.resolve() for p in headers if any(
        (ROOT / DOSSIERS / f) in p.parents or (ROOT / 'bindings/common/distributions' / f) in p.parents
        for f in reasons)]
    queue.extend(sorted(changed_headers))
    seen = set(queue)
    while queue:
        target = queue.pop(0)
        for dependent in sorted(reverse.get(target, [])):
            if dependent in seen:
                continue
            seen.add(dependent)
            queue.append(dependent)
            rel = str(dependent.relative_to(ROOT))
            match = re.search(r'distributions/([^/]+)/', rel)
            if match and match.group(1) in families:
                reasons.setdefault(match.group(1), []).append(
                    f'{rel} includes {target.relative_to(ROOT)}')
    if shared:
        for family in families:
            reasons.setdefault(family, []).append('shared implementation/build effects')
    return sorted(reasons), reasons, shared


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('base', nargs='?', default='master')
    parser.add_argument('--dry-run', action='store_true')
    parser.add_argument('--no-long-double', action='store_true', default=os.getenv('UMVL_TEST_NO_LONG_DOUBLE') == '1')
    args = parser.parse_args()
    os.chdir(ROOT)
    ancestor, paths = changed_paths(args.base)
    families, reasons, shared = select(paths)
    print(f'Base {args.base}; merge base {ancestor}; tier {os.getenv("UMVL_TEST_TIER", "dev")}', flush=True)
    print(f'Selected {len(families)} families: {", ".join(families) or "none"}', flush=True)
    for family in families:
        print(f'  {family}: ' + '; '.join(reasons[family]), flush=True)
    if shared:
        print('Shared effects: running ordinary recipes without family filters.', flush=True)
        for path in shared:
            print('  shared: ' + path, flush=True)
    if args.dry_run or not families:
        return 0
    env = os.environ.copy()
    # Selection belongs to this runner, rather than leftover ad hoc JS filters.
    env.pop('FIT_FAMILY', None)
    env.pop('JS_FAMILIES', None)
    if shared:
        env.pop('UMVL_TEST_FAMILIES', None)
        commands = [['just', 'cpp-test'], ['just', 'test'], ['just', 'js-build'], ['just', 'js-test']]
        if args.no_long_double:
            commands.insert(1, ['just', 'cpp-test-no-long-double'])
    else:
        env['UMVL_TEST_FAMILIES'] = ','.join(families)
        env['UMVL_TEST_TIER'] = env.get('UMVL_TEST_TIER') or 'dev'
        build = 'univariateML-cpp/build'
        commands = []
        builds = [build] + (['build/cpp-no-long-double'] if args.no_long_double else [])
        for build in builds:
            config = ['cmake', '-S', 'univariateML-cpp', '-B', build]
            if shutil.which('ccache'):
                config.append('-DCMAKE_CXX_COMPILER_LAUNCHER=ccache')
            if build != 'univariateML-cpp/build':
                config.append('-DCMAKE_CXX_FLAGS=-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS')
            commands.extend([config, ['cmake', '--build', build, '--target', 'distribution_tests',
                                     '-j', env.get('CMAKE_BUILD_PARALLEL_LEVEL', '1')],
                             ['ctest', '--test-dir', build, '--output-on-failure', '--no-tests=error',
                              '-L', env['UMVL_TEST_TIER'], '-R',
                              '^distribution_(' + '|'.join(map(re.escape, families)) + ')(_slow)?$']])
        commands += [['just', 'test'], ['just', 'js-build'], ['just', 'js-test']]
    results = []
    for command in commands:
        print('+ ' + ' '.join(command), flush=True)
        start = time.monotonic()
        code = subprocess.call(command, env=env)
        if code and command[:2] == ['cmake', '-S']:
            boost = subprocess.check_output(
                ['Rscript', '-e', 'cat(system.file("include", package="BH"))'], env=env
            ).decode().strip()
            if not (Path(boost) / 'boost/version.hpp').is_file():
                print('Neither standalone Boost nor R/BH headers available.', flush=True)
            else:
                code = subprocess.call(command + ['-DBoost_INCLUDE_DIR=' + boost], env=env)
        results.append({'command': command, 'exit': code, 'seconds': round(time.monotonic()-start, 2)})
        if code:
            break
    print('Results: ' + json.dumps(results), flush=True)
    return results[-1]['exit']


if __name__ == '__main__':
    sys.exit(main())
