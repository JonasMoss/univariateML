"""Replay exact frozen binary64 samples with double-width intermediates.
Run under /tmp/umvl-cpu/run; native input files come from run-study.py.
"""
from pathlib import Path
import os,shutil,subprocess,sys
root=Path.cwd();evidence=Path(__file__).resolve().parent;exe=root/'build/scratch/held-study-double'
tmp=root/'build/tmp';tmp.mkdir(parents=True,exist_ok=True)
os.environ.update(TMPDIR=str(tmp),JUST_TEMPDIR=str(tmp),XDG_RUNTIME_DIR=str(tmp))
os.environ.update(CCACHE_DIR=str(Path.home()/'.cache/ccache/univariateML'),CCACHE_BASEDIR=str(root),CCACHE_NOHASHDIR='1')
compiler=([shutil.which('ccache')] if shutil.which('ccache') else [])+['c++']
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
subprocess.run(compiler+['-O2','-std=c++17','-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(evidence/'held-study.cpp'),'-o',str(exe)],check=True)
for name,inputs in [('held-study','held-samples'),('archive','archive-samples')]:
 source=name+'-double.csv'
 with (evidence/source).open('w') as f:subprocess.run([str(exe),'replay',str(evidence/(inputs+'.csv'))],stdout=f,check=True)
 subprocess.run([sys.executable,str(evidence/'reference-check.py'),source,name+'-double-reference.json',inputs+'.csv'],check=True)
 if name=='archive':subprocess.run([sys.executable,str(evidence/'archive-check.py'),source,'archive-double-check.json',inputs+'.csv'],check=True)

subprocess.run([sys.executable,str(evidence/'failed-accuracy.py'),'archive-double.csv','double-failed-accuracy.json'],check=True)
