import subprocess,json,os
from pathlib import Path
base=Path(__file__).resolve().parent;root=base.parents[3];os.chdir(root)
b=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
subprocess.run(['c++','-std=c++17','-O2','-IunivariateML-cpp/include','-I'+b,str(base/'global-probe.cpp'),'-o','build/scratch/zinb-global-probe'],check=True)
g=json.load(open('univariateML-cpp/include/cppunivariate/distributions/zinb/golden.json'))
subprocess.run(['build/scratch/zinb-global-probe'],input=' '.join(map(str,[len(g['data'])]+g['data']))+'\n',text=True,check=True)
