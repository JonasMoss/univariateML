"""Compile only this family's doctests in native and double-width modes.
Run under /tmp/umvl-cpu/run from the repository root after golden generation.
"""
from pathlib import Path
import subprocess,time,json,os,shutil
root=Path.cwd();out=root/'build/scratch';rows=[]
tmp=root/'build/tmp';tmp.mkdir(parents=True,exist_ok=True)
os.environ.update(TMPDIR=str(tmp),JUST_TEMPDIR=str(tmp),XDG_RUNTIME_DIR=str(tmp))
os.environ.update(CCACHE_DIR=str(Path.home()/'.cache/ccache/univariateML'),CCACHE_BASEDIR=str(root),CCACHE_NOHASHDIR='1',CCACHE_MAXSIZE='8G')
compiler=([shutil.which('ccache')] if shutil.which('ccache') else [])+['c++']
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include", package="BH"))'],text=True)
for mode,flags in [('native',[]),('double',['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'])]:
 exe=out/('test-ew-'+mode)
 cmd=compiler+['-O2','-std=c++17',*flags,'-I'+str(root/'univariateML-cpp/include'),'-I'+str(root/'univariateML-cpp/tests'),'-I'+boost,'-DCPPUNIV_DISTRIBUTIONS_DIR="'+str(root/'univariateML-cpp/include/cppunivariate/distributions')+'"',str(root/'univariateML-cpp/tests/distribution_tests_main.cpp'),str(root/'univariateML-cpp/include/cppunivariate/distributions/expweibull/test.cpp'),'-o',str(exe)]
 subprocess.run(cmd,check=True)
 golden_present=(root/'univariateML-cpp/include/cppunivariate/distributions/expweibull/golden.json').exists()
 args=[str(exe),'--test-case=expweibull*']
 if not golden_present:args+=['--test-case-exclude=*golden*']
 begin=time.monotonic();subprocess.run(args,check=True)
 common_exe=out/('common-ew-'+mode)
 subprocess.run(compiler+['-O2','-std=c++17',*flags,'-I'+str(root/'univariateML-cpp/include'),'-I'+str(root/'bindings'),'-I'+boost,str(Path(__file__).parent/'common-check.cpp'),'-o',str(common_exe)],check=True)
 subprocess.run([str(common_exe)],check=True)
 rows.append(dict(mode=mode,seconds=time.monotonic()-begin,golden_present=golden_present,common_binding=True))
 print(mode,'passed',flush=True)
(Path(__file__).parent/'target-check.json').write_text(json.dumps(rows,indent=2)+'\n')
