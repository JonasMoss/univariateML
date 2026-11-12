"""Run under /tmp/umvl-cpu/run from the repository root (one thread).
Baseline: af824a98. No source archive or installed dependency is required.
"""
import subprocess,sys,os,shutil
from pathlib import Path
root=Path.cwd();evidence=Path(__file__).resolve().parent;scratch=root/'build/scratch'
tmp=root/'build/tmp';tmp.mkdir(parents=True,exist_ok=True)
os.environ.update(TMPDIR=str(tmp),JUST_TEMPDIR=str(tmp),XDG_RUNTIME_DIR=str(tmp))
os.environ.update(CCACHE_DIR=str(Path.home()/'.cache/ccache/univariateML'),CCACHE_BASEDIR=str(root),CCACHE_NOHASHDIR='1',CCACHE_MAXSIZE='8G')
compiler=([shutil.which('ccache')] if shutil.which('ccache') else [])+['c++']
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include", package="BH"))'],text=True)
for source in ['held-study','derivative']:
 command=compiler+['-O2','-std=c++17','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(evidence/(source+'.cpp')),'-o',str(scratch/source)]
 subprocess.run(command,check=True)
subprocess.run(compiler+['-O2','-std=c++17','-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(evidence/'derivative.cpp'),'-o',str(scratch/'derivative-double')],check=True)
with (evidence/'derivatives-double.csv').open('w') as f:subprocess.run([str(scratch/'derivative-double')],stdout=f,check=True)
for mode,filename,samples in [('census','held-study.csv','held-samples.csv'),('archive','archive.csv','archive-samples.csv')]:
 args=['replay',str(evidence/samples)] if (evidence/samples).exists() else [mode,str(evidence/samples)]
 with (evidence/filename).open('w') as f:subprocess.run([str(scratch/'held-study'),*args],stdout=f,check=True)
with (evidence/'held-study-budget1000.csv').open('w') as f:subprocess.run([str(scratch/'held-study'),'replay',str(evidence/'held-samples.csv'),'1000'],stdout=f,check=True)
with (evidence/'derivatives.csv').open('w') as f:subprocess.run([str(scratch/'derivative')],stdout=f,check=True)
for script in ['summarize.py','derivative-check.py','reference-check.py','archive-check.py','failed-accuracy.py']:
 subprocess.run([sys.executable,str(evidence/script)],check=True)

subprocess.run([sys.executable,str(evidence/'derivative-check.py'),'derivatives-double.csv','derivative-double-check.json'],check=True)

subprocess.run([sys.executable,str(evidence/'summarize.py'),'held-study-budget1000.csv'],check=True)

subprocess.run([sys.executable,str(evidence/'reference-check.py'),'archive.csv','archive-reference.json','archive-samples.csv'],check=True)
subprocess.run([sys.executable,str(evidence/'double-study.py')],check=True)
