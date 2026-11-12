"""Isolated doctest build: safe alongside the queued CMake builds."""
from pathlib import Path
import subprocess,os,hashlib,json,re
P=Path(__file__).resolve().parent;ROOT=P.parents[3];work=ROOT/'build/scratch/accuracy-focused';work.mkdir(parents=True,exist_ok=True)
families=['burr','betapr','bhatt','gb2','fpareto','ged','invburr','pareto4','paralogis','invparalogis'];include=ROOT/'univariateML-cpp/include';tests=ROOT/'univariateML-cpp/tests'
sources=[tests/'distribution_tests_main.cpp']+[include/('cppunivariate/distributions/'+f+'/test.cpp') for f in families]
patterns=['Bhatt compensated normal*','Bhatt held-sigma near-normal*','Bhatt near-normal width*','Beta prime deeper Gamma face*','Burr weak-coordinate holds*','Comparable beta shapes*','Beta prime comparable large shapes*','Coupled Weibull-limit*','Beta prime compensated finish handles*','Beta prime compensated Gamma departure*','*Weak exponential-limit held powers*','Burr local accuracy retains*','Burr held-power fallback bounds*','Burr unit-power information*','Burr uncertified held-scale root*','Burr large-shape coefficient box*','Burr nonunit held-power fallback*','beta-prime unit-shape fallback meets*','beta-prime final fallback preserves*','Beta prime inverse gamma fallback*','Bhatt conditional location enclosure*','Bhatt normal-member accuracy*','Bhatt weak width curvature*','Bhatt asymmetric location detects*','Bhatt normal endpoint distinguishes*','GED uncertified knot fit*','GB2 small-power delegate*','Feller Pareto shifted small-power delegate*','paralogis exhausted cover*','inverse paralogistic transport keeps*']
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip();env=dict(os.environ,CCACHE_DIR=os.environ['HOME']+'/.cache/ccache/univariateML',CCACHE_BASEDIR=str(ROOT),CCACHE_NOHASHDIR='1')
def hashes():return {str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in list(include.rglob('*.hpp'))+sources}
result={}
for mode in ['native','double']:
 initial=hashes();objects=[];flags=['-std=c++17','-O2','-I'+str(include),'-I'+str(tests),'-I'+boost,'-DCPPUNIV_DISTRIBUTIONS_DIR="'+str(include/'cppunivariate/distributions')+'"']+(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if mode=='double' else [])
 with (work/('build-'+mode+'.log')).open('w') as buildlog:
  for i,source in enumerate(sources):
   print('focused compile',mode,source.parent.name,source.name,flush=True);obj=work/(mode+'-'+str(i)+'.o');objects.append(str(obj));subprocess.run(['ccache','g++',*flags,'-c',str(source),'-o',str(obj)],stdout=buildlog,stderr=subprocess.STDOUT,env=env,check=True)
  exe=work/('tests-'+mode);subprocess.run(['g++',*objects,'-o',str(exe)],stdout=buildlog,stderr=subprocess.STDOUT,check=True)
 assert hashes()==initial,'Source changed during the isolated build'
 completed=subprocess.run([str(exe),'--test-case='+','.join(patterns)],text=True,capture_output=True);(P/('focused-tests-'+mode+'.log')).write_text(completed.stdout+completed.stderr);print(completed.stdout,flush=True)
 count=re.search(r'test cases:\s*(\d+)\s*\|\s*(\d+) passed',completed.stdout)
 result[mode]=dict(exit_code=completed.returncode,cases=int(count[1]) if count else None,passed=int(count[2]) if count else None,sha256=initial)
 (P/'focused-tests.json').write_text(json.dumps(result,indent=2)+'\n');assert completed.returncode==0,(mode,completed.returncode)
