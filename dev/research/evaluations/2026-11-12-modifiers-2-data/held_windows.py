"""Standalone binding check for density-only extreme held windows."""
from pathlib import Path
import subprocess,json
root=Path.cwd();scratch=root/'build/scratch';cpp=scratch/'held-windows.cpp';exe=scratch/'held-windows'
cpp.write_text(r'''
#include "bindings/common/modifiers.hpp"
#include <iostream>
#include <iomanip>
int main() {
 const std::vector<double> z{0,0},o{1,1},b{0,1};
 const auto huge=umvl_binding::fit_modifier("pois",{z.data(),z.size()},{{"lambda",{true,1e300}}},"none",true,0,INFINITY,1e-10,1);
 const auto tiny=umvl_binding::fit_modifier("pois",{o.data(),o.size()},{{"lambda",{true,1e-300}}},"none",true,0,INFINITY,1e-10,1);
 const auto bin=umvl_binding::fit_modifier("binom",{b.data(),b.size()},{{"size",{true,5}},{"prob",{true,.3}}},"none",true,0,5,1e-10,1);
 if(!huge.error.empty() || !tiny.error.empty() || !bin.error.empty())return 1;
 std::cout<<std::setprecision(17)<<huge.logLik<<" "<<tiny.logLik<<" "<<bin.logLik<<"\n";
}
''')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
subprocess.run(['c++','-std=c++17','-O2','-I'+str(root),'-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(cpp),'-o',str(exe)],check=True)
values=list(map(float,subprocess.check_output([str(exe)],text=True).split()))
import math
expected=[-2e300,2*math.log(1e-300),9*math.log(.7)+math.log(5*.3)]
for a,b in zip(values,expected):assert abs(a-b)<=1e-12*max(1,abs(b)),(a,b)
(root/'dev/research/evaluations/2026-11-12-modifiers-2-data/held-windows.json').write_text(json.dumps(dict(logLik=values,closed_forms=expected),indent=2)+'\n')
print('Three extreme held-window checks passed')
