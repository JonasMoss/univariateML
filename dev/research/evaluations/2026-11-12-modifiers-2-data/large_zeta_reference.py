"""Exact uniform-MLE oracle, naive-statistic ablation, and complete zeta doctests."""
from pathlib import Path
import json,subprocess
root=Path.cwd();p=Path(__file__).resolve().parent;scratch=root/'build/scratch'
header=(root/'univariateML-cpp/include/cppunivariate/distributions/zeta/truncation.hpp').read_text()
a=header.index('inline ZetaWindowResult zeta_window_mle(');b=header.index('// Conditional evaluation',a)
fn=header[a:b].replace('zeta_window_mle(','zeta_window_mle_naive(',1)
first=fn.index('  double target=');last=fn.index('  target/=x.size();',first)
fn=fn[:first]+'''  double target=0,centered_target=0;
  bool lower=true,upper=true;
  for(int v:x) {if(v<1 || v>N)return out;target+=std::log(double(v));centered_target+=zeta_window_detail::log_ratio(v,N);lower=lower&&v==1;upper=upper&&v==N;}
'''+fn[last:]
cpp=scratch/'zeta-large-oracle.cpp'
cpp.write_text('#include "cppunivariate/distributions/zeta/truncation.hpp"\n#include <iostream>\n#include <iomanip>\nnamespace cppunivariate {\n'+fn+'}\n'+r'''
int main(){std::cout<<std::setprecision(17);for(int m:{1000,100000,500000,1000000}){
 std::vector<int>x;for(int k=1;k<=4;++k)x.insert(x.end(),m,k);
 const auto r=cppunivariate::zeta_window_mle({x.data(),x.size()},1,4,NAN,1e-10,400);
 const auto n=cppunivariate::zeta_window_mle_naive({x.data(),x.size()},1,4,NAN,1e-10,400);
 std::cout<<x.size()<<" "<<int(r.status)<<" "<<r.s<<" "<<n.s<<"\n";}}
''')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
includes=['-I'+str(root/'univariateML-cpp/include'),'-I'+str(root/'univariateML-cpp/tests'),'-I'+boost]
testcpp=scratch/'zeta-family-only.cpp'
testcpp.write_text('#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN\n#include "doctest.h"\n#include "'+str(root/'univariateML-cpp/include/cppunivariate/distributions/zeta/test.cpp')+'"\n')
runs=[]
for label,flags in [('standard',[]),('no-long-double',['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'])]:
 exe=scratch/('zeta-large-'+label)
 subprocess.run(['c++','-std=c++17','-O2',*flags,*includes,str(cpp),'-o',str(exe)],check=True)
 rows=[]
 for line in subprocess.check_output([str(exe)],text=True).splitlines():
  n,status,value,naive=map(float,line.split());assert status==0 and abs(value)<=1e-10,(label,line)
  rows.append(dict(n=int(n),status=int(status),computed=value,naive_ablation=naive,reference=0))
 assert abs(rows[-1]['naive_ablation'])>1e-10
 binary=scratch/('zeta-tests-'+label)
 subprocess.run(['c++','-std=c++17','-O2',*flags,*includes,'-DCPPUNIV_DISTRIBUTIONS_DIR="'+str(root/'univariateML-cpp/include/cppunivariate/distributions')+'"',str(testcpp),'-o',str(binary)],check=True)
 test=subprocess.run([str(binary),'--no-colors','--no-path-filenames'],text=True,stdout=subprocess.PIPE,stderr=subprocess.STDOUT)
 assert test.returncode==0,test.stdout
 runs.append(dict(mode=label,rows=rows,doctest_exit=test.returncode,doctest_summary=test.stdout.splitlines()[-5:]))
 print(label,': four uniform-MLE oracles and complete zeta doctests passed',flush=True)
(p/'large-zeta-reference.json').write_text(json.dumps(runs,indent=2)+'\n')
