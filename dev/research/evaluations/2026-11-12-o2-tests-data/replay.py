#!/usr/bin/env python3
"""Rebuild the frozen baseline probe without archiving its source tree.
Run through /tmp/umvl-cpu/run; pass --current for the working-tree headers.
Compiler binaries and extracted baseline files stay in ignored build/scratch.
"""
import argparse
import json
import os
from pathlib import Path
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument('--current', action='store_true')
args = parser.parse_args()
root = Path(__file__).resolve().parents[4]
os.chdir(root)
temporary = root / "build/tmp"
temporary.mkdir(parents=True, exist_ok=True)
for variable in ("TMPDIR", "JUST_TEMPDIR", "XDG_RUNTIME_DIR"):
    os.environ[variable] = str(temporary)
baseline = '217ab8b533cac25ccc8eba8d66eccde1b376f0b4'
scratch = root / 'build/scratch/o2-replay'
scratch.mkdir(parents=True, exist_ok=True)
if args.current:
    include = root / 'univariateML-cpp/include'
else:
    include = scratch / 'baseline/univariateML-cpp/include'
    paths = subprocess.check_output(['git', 'ls-tree', '-r', '--name-only', baseline,
                                    'univariateML-cpp/include']).decode().splitlines()
    for name in paths:
        if not name.endswith('.hpp'):
            continue
        out = scratch / 'baseline' / name
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_bytes(subprocess.check_output(['git', 'show', f'{baseline}:{name}']))
probe = Path(__file__).with_name('tail_probe.cpp')
boost = subprocess.check_output(['Rscript', '-e', 'cat(system.file("include", package="BH"))']).decode().strip()
records = []
for mode in ('native', 'no-long-double'):
    for opt in (['-O0'], ['-O1'], ['-O2'], ['-O2', '-fno-inline']):
        binary = scratch / 'probe'
        command = ['g++', '-g', '-std=gnu++17', '-DNDEBUG', *opt, '-I'+str(include), '-I'+boost]
        if mode != 'native':
            command += ['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS']
        subprocess.run([*command, str(probe), '-o', str(binary)], check=True)
        output = subprocess.check_output([str(binary)]).decode()
        records.append(dict(mode=mode, flags=command[1:], output=output))
print(json.dumps(dict(baseline=baseline, current=args.current,
                     compiler=subprocess.check_output(['g++','--version']).decode().splitlines()[0],
                     records=records), indent=2))
