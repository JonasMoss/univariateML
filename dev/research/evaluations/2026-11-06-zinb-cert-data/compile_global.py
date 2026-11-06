import subprocess,os,sys
from pathlib import Path
base=Path(__file__).resolve().parent;root=base.parents[3];os.chdir(root)
b=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
subprocess.run(['c++','-std=c++17','-O2','-Wall','-Wextra','-Wpedantic','-IunivariateML-cpp/include','-I'+b,str(base/'global-check-probe.cpp'),'-o','build/scratch/zinb-global-check'],check=True)
subprocess.run(['python3',str(base/'check_global.py'),'build/scratch/zinb-global-check'],check=True)
