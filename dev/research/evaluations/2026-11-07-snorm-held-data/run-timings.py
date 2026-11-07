"""Run under /tmp/umvl-cpu/run, from this worktree; no nested reservation."""
import os
from pathlib import Path
import subprocess
root=Path(__file__).resolve().parents[4]
os.chdir(root)
data=Path(__file__).resolve().parent
scratch=root/'build/scratch/snorm-timings';scratch.mkdir(parents=True,exist_ok=True)
os.environ.update({k:str(root/'build/tmp') for k in ('TMPDIR','JUST_TEMPDIR','XDG_RUNTIME_DIR')})
source=(data/'timing.cpp').read_text()
original=subprocess.check_output(['git','show','7c52f905:univariateML-cpp/include/cppunivariate/distributions/snorm/mle.hpp'],text=True)
(scratch/'baseline.hpp').write_text(original.replace('#include "../../','#include "cppunivariate/'))
(scratch/'baseline.cpp').write_text(source.replace('"cppunivariate/distributions/snorm/mle.hpp"','"baseline.hpp"'))
for label,src,flags in [('before',scratch/'baseline.cpp',[]),('after',data/'timing.cpp',[]),('after-double',data/'timing.cpp',['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'])]:
    exe=scratch/label
    subprocess.run(['ccache','c++','-O2','-std=c++17',*flags,'-I'+str(root/'univariateML-cpp/include'),'-I'+str(scratch),str(src),'-o',str(exe)],check=True)
    (data/f'timing-{label}.txt').write_text(subprocess.check_output([str(exe)],text=True))
