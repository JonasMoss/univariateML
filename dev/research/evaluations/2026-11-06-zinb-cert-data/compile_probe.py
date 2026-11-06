import os,subprocess
from pathlib import Path
root=Path(__file__).resolve().parents[4]
os.chdir(root)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
subprocess.run(['ccache','c++','-std=c++17','-O2','-Wall','-Wextra','-Wpedantic','-IunivariateML-cpp/include','-I'+boost,str(Path(__file__).with_name('probe.cpp')),'-o','build/scratch/zinb-probe'],check=True)
subprocess.run(['python3',str(Path(__file__).with_name('check.py')),'build/scratch/zinb-probe'],check=True)
