"""Add CPU clocks to the already-running experiment's last pair.

Invoke under one slot before repetition 2. Atomic replacement leaves any
running repetition mapped to its original executable; no fit is interrupted.
A clean reproduce.py run includes CPU timing in every repetition.
"""
from pathlib import Path
import subprocess,os
p=Path(__file__).resolve().parent;scratch=Path('build/scratch/emg-perf')
for version in ['before','after']:
    overlay=scratch/f'{version}-instrumented';binary=scratch/f'{version}-instrumented-bin'
    temporary=binary.with_suffix('.new')
    subprocess.run(['c++','-std=c++17','-O2','-DEMG_PERF_PROFILE',f'-I{overlay}','-IunivariateML-cpp/tests',str(p/'benchmark.cpp'),'-o',str(temporary)],check=True)
    os.replace(temporary,binary)
print('CPU-clock binaries ready for the last timing pair',flush=True)
