"""Generate a native baseline census of every family and held subset.
Uses binding defaults; run under one CPU slot via census.sh.
"""
from pathlib import Path
import re
headers=sorted(Path('bindings/common/distributions').glob('*/fit.hpp'))
output=['#include "json_loader.hpp"', '#include <chrono>', '#include <iostream>', '#include <iomanip>']
output += [f'#include "common/distributions/{p.parent.name}/fit.hpp"' for p in headers]
output += ['int main() {', 'std::cout << std::setprecision(17);']
for p in headers:
    family=p.parent.name
    signature=re.search(r'inline FitResult fit_'+family+r'\(([^\n]+)\)\s*\{',p.read_text()).group(1)
    names=re.findall(r'Held (\w+)_arg',signature)
    assert names,(family,signature)
    output += ['{',f'const std::string id="{family}";',
      'const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json");',
      'std::vector<double> data;for(int j=0;j<500;++j)data.push_back(g.data[j%g.data.size()]);']
    if 'cppunivariate::ispan' in signature:
        output += ['std::vector<int> ix;for(double v:data)ix.push_back(static_cast<int>(v));',
                   'const cppunivariate::ispan x{ix.data(),ix.size()};']
    else: output += ['const cppunivariate::span x{data.data(),data.size()};']
    output += [f'for(unsigned mask=0;mask<{1<<len(names)};++mask) {{',
               'std::string held;']
    for bit,name in enumerate(names):
        output += [f'if(mask&{1<<bit}){{if(!held.empty())held+=",";held+="{name}";}}']
    output += ['const auto start=std::chrono::steady_clock::now();',
       f'const auto r=umvl_binding::fit_{family}(x,'+', '.join(f'umvl_binding::Held{{bool(mask&{1<<bit}),g.estimates.at("{name}")}}' for bit,name in enumerate(names))+');',
       'const double elapsed=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();',
       'std::string error=r.error;for(char& c:error)if(c==\'\\t\'||c==\'\\n\')c=\' \';',
       'std::cout<<id<<"\\t"<<mask<<"\\t"<<(held.empty()?"none":held)<<"\\t"<<data.size()<<"\\t"<<elapsed<<"\\t"<<(error.empty()?"ok":"stop")<<"\\t"<<r.logLik<<"\\t"<<error<<std::endl;',
       '}', '}']
output += ['}']
Path('build/scratch/census.cpp').write_text('\n'.join(output)+'\n')
print(f'Generated {len(headers)} families.')
