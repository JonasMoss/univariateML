"""Build the frozen baseline and current timing programs from repository root."""
import os
from pathlib import Path
import subprocess
root = Path.cwd()
scratch = root / 'build/scratch'
scratch.mkdir(parents=True, exist_ok=True)
(root / 'build/tmp').mkdir(parents=True, exist_ok=True)
for name in ('TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR'):
    os.environ[name] = str(root / 'build/tmp')
source = subprocess.check_output([
    'git','show',
    '30d60e58a168463bbd0a01efc15d8490b0b01091:univariateML-cpp/include/cppunivariate/distributions/bbinom/mle.hpp'
], text=True)
(scratch / 'baseline.hpp').write_text(source.replace('#include "../../','#include "cppunivariate/'))
current = root / 'dev/research/evaluations/2026-11-07-bbinom-face-data/timing.cpp'
(scratch / 'bench-before.cpp').write_text(current.read_text().replace(
    '"cppunivariate/distributions/bbinom/mle.hpp"','"baseline.hpp"'))
boost = subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
for name, path in [('before',scratch/'bench-before.cpp'),('after',current)]:
    subprocess.run(['c++','-O2','-std=c++17',f'-I{boost}',
                    '-IunivariateML-cpp/include',str(path),'-o',
                    str(scratch/f'bench-{name}-final')],check=True)
