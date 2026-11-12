"""Instrument copies of the retained native tests; never edit their sources.
Run under /tmp/umvl-cpu/run. Requires the already installed C++ compiler and BH.
"""
from pathlib import Path
import re,subprocess,json,os,gzip
root=Path.cwd();out=root/'build/scratch/native';out.mkdir(parents=True,exist_ok=True)
summary=[]
ids=['tbinom','tnorm','tpois','zib','zinb','zip','zipf','zmbinom','zmgeom','zmlogarithmic','zmnbinom','zmpois','ztbinom','ztgeom','ztnbinom','ztpois']
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
logpath=root/'dev/research/evaluations/2026-11-12-modifiers-2-data/native-cases.jsonl'
with logpath.open('w') as log:
 for family in ids:
  folder=root/'univariateML-cpp/include/cppunivariate/distributions'/family
  header=(folder/'mle.hpp').read_text();wrappers=[];names=[]
  for name in [family+'_mle']+(['tbinom_design_mle'] if family=='tbinom' else []):
   for match in list(re.finditer(r'inline\s+(\w+(?:::\w+)*)\s+'+name+r'\(',header))[:1]:
    pos=match.end();end=pos;depth=1
    while depth:
     if header[end]=='(':depth+=1
     elif header[end]==')':depth-=1
     end+=1
    args=header[pos:end-1];parts=args.split(',');variables=[]
    for part in parts:
     left=part.split('=')[0].strip();variables.append(re.search(r'(\w+)\s*$',left)[1])
    if variables[0]!='x':raise RuntimeError(args)
    emit='emit("'+family+'","'+name+'",x,{' + ','.join('{"'+v+'",double('+v+')}' for v in variables[1:])+'});'
    wrappers.append('inline '+match[1]+' capture_'+name+'('+args+') {'+emit+'return '+name+'('+','.join(variables)+');}')
   names.append(name)
  source='''#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "cppunivariate/special/tails.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include <iostream>
#include <map>
#include <iomanip>
namespace cppunivariate {
template<class Span> void emit(const std::string& family,const std::string& function,Span x,const std::vector<std::pair<std::string,double>>& args) {
 std::cout<<"{\\"family\\":\\""<<family<<"\\",\\"function\\":\\""<<function<<"\\",\\"data\\":[";
 bool first=true;
 // Histogram prevents repeated count samples from making huge evidence files.
 std::map<double,std::size_t> counts;std::size_t nans=0,pinf=0,ninf=0;
 for(auto v:x) {if(std::isnan(double(v)))++nans;else if(v==INFINITY)++pinf;else if(v==-INFINITY)++ninf;else ++counts[v];}
 for(auto p:counts) {if(!first)std::cout<<",";first=false;std::cout<<"[";if(std::isfinite(p.first))std::cout<<std::setprecision(17)<<p.first;else std::cout<<"null";std::cout<<","<<p.second<<"]";}
 for(auto p:std::vector<std::pair<std::string,std::size_t>>{{"null",nans},{"\\\"Inf\\\"",pinf},{"\\\"-Inf\\\"",ninf}})if(p.second) {if(!first)std::cout<<",";first=false;std::cout<<"["<<p.first<<","<<p.second<<"]";}
 std::cout<<"],\\"args\\":{";first=true;
 for(auto p:args) {if(!first)std::cout<<",";first=false;std::cout<<"\\""<<p.first<<"\\":";if(std::isfinite(p.second))std::cout<<std::setprecision(17)<<p.second;else std::cout<<"\\""<<(std::isnan(p.second)?"NaN":p.second>0?"Inf":"-Inf")<<"\\"";}
 std::cout<<"}}\\n";
}
'''+ '\n'.join(wrappers)+'\n}\n'+'\n'.join('#define '+n+' capture_'+n for n in names)+'\n#include "'+str(out/(family+'-original.cpp'))+'"\n'
  original=subprocess.check_output(['git','show','fe17e3b8:'+str((folder/'test.cpp').relative_to(root))],text=True)
  (out/(family+'-original.cpp')).write_text(original)
  source=source.replace('#include \"cppunivariate/cppunivariate.hpp\"','#include \"'+str(folder/'mle.hpp')+'\"')
  cpp=out/(family+'.cpp')
  if not cpp.exists() or cpp.read_text()!=source:cpp.write_text(source)
  binary=out/family
  command=['c++','-std=c++17','-O2','-DCPPUNIV_DISTRIBUTIONS_DIR=\"'+str(root/'univariateML-cpp/include/cppunivariate/distributions')+'\"','-I'+str(folder),'-I'+str(root/'univariateML-cpp/tests'),'-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(cpp),'-o',str(binary)]
  subprocess.run(command,check=True)
  result=subprocess.run([str(binary),'--no-colors','--no-path-filenames'],stdout=subprocess.PIPE,text=True)
  for line in result.stdout.splitlines():
   if line.startswith('{"family"'):log.write(line+'\n')
  print(family,'test exit',result.returncode,flush=True)
  summary.append(dict(family=family,baseline='fe17e3b8',exit=result.returncode,calls=sum(line.startswith('{"family"') for line in result.stdout.splitlines())))
with logpath.open('rb') as src,gzip.open(str(logpath)+'.gz','wb') as dst:dst.write(src.read())
logpath.unlink()
(root/'dev/research/evaluations/2026-11-12-modifiers-2-data/native-test-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
assert all(r['exit']==0 for r in summary)
