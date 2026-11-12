"""Rebuild the two baseline headers from Git, then sample each direct C++ fit.
Run through /tmp/umvl-cpu/run; no nested slot reservations.
"""
import os,re,subprocess
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]; HERE=Path(__file__).resolve().parent
os.environ.update(TMPDIR=str(ROOT/'build/tmp'),JUST_TEMPDIR=str(ROOT/'build/tmp'),XDG_RUNTIME_DIR=str(ROOT/'build/tmp'))
BASE='bb46078757dad3538bfb263086d4e72cde163330'
scratch=ROOT/'build/scratch/fpareto-profile';scratch.mkdir(exist_ok=True)
for family in ['gb2','fpareto']:
 path=Path('univariateML-cpp/include/cppunivariate/distributions')/family/'mle.hpp'
 source=subprocess.check_output(['git','show',f'{BASE}:{path}'],text=True,cwd=ROOT)
 def inc(m):
  target=(ROOT/path.parent/m[1]).resolve()
  if m[1]=='../gb2/mle.hpp':target=scratch/'profile-gb2.hpp'
  return '#include "'+str(target)+'"'
 source=re.sub(r'#include "([^"]+)"',inc,source)
 (scratch/f'profile-{family}.hpp').write_text(source)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include", package="BH"))'],text=True)
exe=scratch/'profile'
subprocess.run(['g++','-std=c++17','-O2','-pg','-I'+str(scratch),'-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,str(HERE/'profile.cpp'),'-o',str(exe)],check=True)
for case in [1599,1600,10717]:
 env=os.environ.copy();env['GMON_OUT_PREFIX']=str(scratch/f'gmon-{case}')
 subprocess.run([str(exe),str(case)],env=env,check=True)
 files=list(scratch.glob(f'gmon-{case}.*'))
 with (HERE/f'profile-{case}.txt').open('w') as out:
  subprocess.run(['gprof',str(exe),str(files[-1])],stdout=out,check=True)
