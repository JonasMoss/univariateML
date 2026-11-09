"""Build isolated before/after headers; production sources never instrumented.
Usage: python3 profile.py baseline|final [instrument]
"""
import pathlib,re,json,sys,gzip
root=pathlib.Path.cwd();stage=sys.argv[1];instrument=len(sys.argv)>2
folder=root/'dev/research/evaluations/2026-11-09-gev-perf-data'
dest=root/f'build/scratch/perf-{stage}{"-profile" if instrument else ""}'
dest.mkdir(parents=True,exist_ok=True);base=root/'univariateML-cpp/include'
for fam in ['gev','frechet']:
 for src in (base/f'cppunivariate/distributions/{fam}').glob('*.hpp'):
  s=gzip.decompress((folder/'baseline-joint.hpp.gz').read_bytes()).decode() if stage=='baseline' and src.name=='joint_certificate.hpp' else src.read_text()
  def inc(m):
   target=(src.parent/m[1]).resolve()
   if target.parent.name in ['gev','frechet']:target=dest/target.relative_to(base)
   return '#include "'+str(target)+'"'
  s=re.sub(r'#include "([^"]+)"',inc,s)
  if instrument:
   for name in ['evaluate','nuisance_evaluate','gap_evaluate','inner','region','negative_upper','coefficients','public_bounds','precise','face_upper','gev_profile','gev_joint_certificate']:
    pat=r'((?:inline [^\n]+\b'+name+r'\([^;]*?\)\s*)\{)'
    s=re.sub(pat,lambda m:m[0]+' PerfScope perf_scope("'+name+'");',s)
   if src.name=='joint_certificate.hpp':
    s=s.replace('if(radius<=.25) {','if(radius<=.25) { PerfScope perf_series("series");')
    s=s.replace('const Slab s=pending.back();','PerfScope perf_slab("slab");\n    const Slab s=pending.back();')
    s=s.replace('struct Slab {double lo,hi;};','struct Slab {double lo,hi; int depth=0;};')
    s=s.replace('pending.pop_back();++evaluations;','pending.pop_back();++evaluations; perf_depth[s.depth]++;')
    s=s.replace('pending.push_back({s.lo,mid});pending.push_back({mid,s.hi});','pending.push_back({s.lo,mid,s.depth+1});pending.push_back({mid,s.hi,s.depth+1});')
    s=s.replace('for(;steps<maxit;++steps) {','for(;steps<maxit;++steps) { PerfScope perf_step("inner_bracket");',1)
    s=s.replace('for(;steps<maxit;++steps) {\n    const double mid','for(;steps<maxit;++steps) { PerfScope perf_step("inner_refine");\n    const double mid')
    s=s.replace('for(;root_steps<maxit && evaluations<cap;++root_steps) {','for(;root_steps<maxit && evaluations<cap;++root_steps) { PerfScope perf_isolation("isolation");')
    s=s.replace('for(int j=0;j<maxit;++j) {\n    const auto box=bounds','for(int j=0;j<maxit;++j) { PerfScope perf_newton("interval_newton");\n    const auto box=bounds')
  target=dest/src.relative_to(base);target.parent.mkdir(parents=True,exist_ok=True);target.write_text(s)
header='''#include <chrono>
#include <map>
#include <string>
#include <iostream>
struct PerfValue { long long calls=0; double seconds=0; };
inline std::map<std::string,PerfValue> perf_values;
inline std::map<int,long long> perf_depth;
struct PerfScope { std::string name; std::chrono::steady_clock::time_point start=std::chrono::steady_clock::now(); PerfScope(const char* n):name(n){} ~PerfScope(){auto& v=perf_values[name];++v.calls;v.seconds+=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();}};
'''
(dest/'instrument.hpp').write_text(header)
s=(root/'dev/research/evaluations/2026-11-08-gev-allfree-data/joint-benchmark.cpp').read_text()
if instrument:
 s=s.replace('const int repeats=job<2?3:1;','const int repeats=1; perf_values.clear();perf_depth.clear();')
 s=s.replace('  }\n}', '''    std::cerr<<"case "<<job<<"\\n";
    for(auto p:perf_values)std::cerr<<p.first<<" "<<p.second.calls<<" "<<p.second.seconds<<"\\n";
    for(auto p:perf_depth)std::cerr<<"depth "<<p.first<<" "<<p.second<<"\\n";
  }
}''')
(dest/'benchmark.cpp').write_text(s)
for family in ['gev','frechet']:
 g=json.loads((base/f'cppunivariate/distributions/{family}/golden.json').read_text())
 with (root/'build/scratch/evt-joint-golden.hpp').open('w' if family=='gev' else 'a') as f:f.write('const std::vector<double> '+family+'_golden{'+','.join(map(repr,g['data']))+'};\n')
s=(root/'dev/research/evaluations/2026-11-08-gev-allfree-data/joint-seeded.cpp').read_text()
s=s.replace('#include <random>','#include <random>\n#include <chrono>')
s=s.replace('auto f=cppunivariate::gev_mle','auto start=std::chrono::steady_clock::now();auto f=cppunivariate::gev_mle')
s=s.replace('<<std::endl;if(f.status', '<<" coef "<<f.mu<<" "<<f.sigma<<" "<<f.xi<<" ll "<<f.logLik<<" restricted "<<f.restricted<<" seconds "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<std::endl;if(f.status')
(dest/'seeded.cpp').write_text(s)
print(dest)
