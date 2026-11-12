"""Exact uniform MLE: distinguish requested precision from a rounded score root."""
from pathlib import Path
import json,subprocess,sys
root=Path.cwd();dest=Path(__file__).resolve().parent;scratch=root/'build/scratch'
source=scratch/'zeta-accuracy-controls.cpp';exe=source.with_suffix('')
source.write_text(r'''
#include "cppunivariate/distributions/zeta/truncation.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);
 for(int repeats:{1,1000,1000000}) {
  std::vector<int>x;for(int k=1;k<=4;++k)x.insert(x.end(),repeats,k);
  for(double tol:{1e-10,1e-14,1e-20}) {
   const auto r=cppunivariate::zeta_window_mle({x.data(),x.size()},1,4,NAN,tol,400);
   std::cout<<x.size()<<" "<<tol<<" "<<int(r.status)<<" "<<r.s<<"\n";
  }
 }
 const std::vector<int>x{1,2,3,4};
 const auto held=cppunivariate::zeta_window_mle({x.data(),x.size()},1,4,0,1e-20,400);
 std::cout<<"held 1e-20 "<<int(held.status)<<" "<<held.s<<"\n";
}
''')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
subprocess.run(['c++','-std=c++17','-O2','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(source),'-o',str(exe)],check=True)
rows=[]
for line in subprocess.check_output([str(exe)],text=True).splitlines():
    n,tol,status,s=line.split();tol=float(tol);status=int(status);s=float(s)
    rows.append(dict(n=n,tol=tol,status=status,s=s,reference=0,wrong_Ok=status==0 and abs(s)>tol))
before='--before' in sys.argv
(dest/('zeta-accuracy-controls-before.json' if before else 'zeta-accuracy-controls.json')).write_text(json.dumps(rows,indent=2)+'\n')
print(json.dumps(rows,indent=2))
if not before:
    assert not any(r['wrong_Ok'] for r in rows)
    assert all(r['status']==0 for r in rows if r['tol']==1e-10 or r['n']=='held')
