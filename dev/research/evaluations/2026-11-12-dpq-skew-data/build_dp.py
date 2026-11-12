import study,subprocess
p=study.W/'probe.cpp';s=p.read_text();import re
s=re.sub(r'if\(op==3\)\{.*?continue;\}', '',s)
s=re.sub(r'#include "cppunivariate/distributions/[^/]+/mle.hpp"\n','',s)
(study.W/'dp.cpp').write_text(s)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
for mode,flags in [('native',[]),('double',['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'])]:
 subprocess.run(['c++','-std=c++17','-O2',*flags,'-I'+str(study.ROOT/'univariateML-cpp/include'),'-I'+boost,str(study.W/'dp.cpp'),'-o',str(study.W/('dp-'+mode))],check=True)
