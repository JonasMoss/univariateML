import os,pathlib,shutil,subprocess
root=pathlib.Path.cwd();out=root/'build/scratch/count-test';out.mkdir(exist_ok=True)
os.environ.update(CCACHE_DIR=str(pathlib.Path.home()/'.cache/ccache/univariateML'),CCACHE_BASEDIR=str(root),CCACHE_NOHASHDIR='1')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
compiler=([shutil.which('ccache')] if shutil.which('ccache') else [])+['c++']
flags=['-std=c++17','-O1','-I'+str(root/'univariateML-cpp/include'),'-I'+str(root/'univariateML-cpp/tests'),'-I'+boost,'-DCPPUNIV_DISTRIBUTIONS_DIR="'+str(root/'univariateML-cpp/include/cppunivariate/distributions')+'"']
sources=[root/'univariateML-cpp/tests/distribution_tests_main.cpp']+[root/f'univariateML-cpp/include/cppunivariate/distributions/{f}/test.cpp' for f in ['bbinom','hyper','nhyper','tbinom','ztbinom','zib','zmbinom']]
objects=[]
for i,source in enumerate(sources):
 obj=out/f'{i}.o';subprocess.run(compiler+flags+['-c',str(source),'-o',str(obj)],check=True);objects.append(str(obj))
binary=out/'tests';subprocess.run(['c++',*objects,'-o',str(binary)],check=True)
subprocess.run([str(binary),'--test-case-exclude=*oracle*,*multistart*,*replication*,*large sample*,*large-sample*,*every held golden subset*,*slow*reference*,*240 seeded*'],check=True)
