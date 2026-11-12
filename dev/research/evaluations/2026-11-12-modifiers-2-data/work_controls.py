"""Check explicit work controls independently of the retained default shim."""
from pathlib import Path
import json,subprocess,sys,math
root=Path.cwd();dest=Path(__file__).resolve().parent
source=root/'build/scratch/tbinom-controls.cpp';exe=source.with_suffix('')
source.write_text(r'''
#include "cppunivariate/distributions/tbinom/mle.hpp"
#include <iostream>
#include <iomanip>
int main() {
 std::cout<<std::setprecision(17);const std::vector<int> x{0,1,2,3};
 for(int limit:{400,511,512,513}) {
  const auto r=cppunivariate::tbinom_design_mle({x.data(),x.size()},-1,INFINITY,1e308,NAN,1e-10,limit);
  std::cout<<limit<<" "<<int(r.status)<<" "<<r.prob<<" "<<r.logLik<<"\n";
 }
 const auto old=cppunivariate::tbinom_design_mle({x.data(),x.size()},-1,INFINITY,1e308);
 std::cout<<"legacy "<<int(old.status)<<" "<<old.prob<<" "<<old.logLik<<"\n";
}
''')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
subprocess.run(['c++','-std=c++17','-O2','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(source),'-o',str(exe)],check=True)
rows=[]
for line in subprocess.check_output([str(exe)],text=True).splitlines():
    limit,status,p,ll=line.split();p=float(p);ll=float(ll)
    rows.append(dict(limit=limit,status=int(status),prob=p if math.isfinite(p) else None,logLik=ll if math.isfinite(ll) else None))
before='--before' in sys.argv
(dest/('work-controls-before.json' if before else 'work-controls.json')).write_text(json.dumps(rows,indent=2)+'\n')
print(json.dumps(rows,indent=2))
if not before:
    for row in rows:
        if row['limit']=='legacy':
            assert row['status']==5,row
        else:
            assert row['status']==0,row
            assert abs(row['prob']/(1.5e-308)-1)<1e-10,row
            ref=6*math.log(1.5)-6-math.lgamma(1)-math.lgamma(2)-math.lgamma(3)-math.lgamma(4)
            assert abs(row['logLik']-ref)<1e-10,row
