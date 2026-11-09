#!/usr/bin/env python3
"""Generate paired standalone benchmark; baseline is read from dcf0832a.
Run from repository root: python3 .../benchmark.py baseline|after
Compile with -std=c++17 -O2 -IunivariateML-cpp/include and the BH include
directory; run emitted build/scratch/slash-perf/<variant> DATA.txt with slot lock.
CSV: input,held_mask,seconds,status,budget,mu,sigma,logLik,profiles,
score_passes,score_observations,certificate_cells,residual_bounds,profile_seconds,
score_seconds,density_seconds[,max_subdivision_depth].
Mask bit 1 holds mu; bit 2 holds sigma. Timings include private instrumentation.
"""
import json, math, pathlib, random, sys
root=pathlib.Path.cwd(); out=root/'build/scratch/slash-perf'; out.mkdir(parents=True,exist_ok=True)
variant=sys.argv[1]
family='univariateML-cpp/include/cppunivariate/distributions/slash/'
src=((pathlib.Path(__file__).parent/'baseline-mle.hpp').read_text() if variant=='baseline' else (root/family/'mle.hpp').read_text())
if '--coarse-grid' in sys.argv:
    a=src.index('  for(std::size_t i=0;i<y.size();++i) {\n    grid.push_back(y[i]);')
    b=src.index('  if(std::isnan(sigma)&&std::isfinite(boundary))',a)
    src=src[:a]+'  for(std::size_t i=0;i<=128;++i)grid.push_back(y[(y.size()-1)*i/128]);\n'+src[b:]
    variant+='-coarse-grid'
# Instrument only private copies; no counters or clocks in production.
import re
src=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((root/family/m[1]).resolve())+'"',src)
src=src.replace('namespace cppunivariate {','''#include <chrono>
namespace perf { inline unsigned long long profiles=0, scores=0, observations=0, cells=0, residuals=0; inline double scores_time=0,density_time=0; inline double profile_time=0; struct Timer { double& elapsed; Timer(double& value=profile_time):elapsed(value){} std::chrono::steady_clock::time_point t=std::chrono::steady_clock::now(); ~Timer(){elapsed+=std::chrono::duration<double>(std::chrono::steady_clock::now()-t).count();} }; }
namespace cppunivariate {''',1)
src=src.replace('double g=0;','perf::Timer score_timer(perf::scores_time); ++perf::scores; perf::observations+=' + ('log_distance.size()' if 'struct ScaleScore' in src else 'x.size()') + '; double g=0;',1)
src=src.replace('SlashResult operator()(double m) const {','SlashResult operator()(double m) const { ++perf::profiles; perf::Timer timer;')
src=src.replace('double g=0;\n        for(double d:log_distance)', 'perf::Timer score_timer(perf::scores_time); ++perf::scores; perf::observations+=log_distance.size(); double g=0;\n        for(double d:log_distance)')
src=src.replace('while(!stack.empty()) {','while(!stack.empty()) { ++perf::cells;')
src=src.replace('maxit = static_cast<int>(solver_budget(maxit));\n  const std::size_t n=d.size()', '++perf::residuals; maxit = static_cast<int>(solver_budget(maxit));\n  const std::size_t n=d.size()')
src=src.replace('double ll=0;for(double v:x)', 'perf::Timer density_timer(perf::density_time); double ll=0;for(double v:x)')
src=src.replace('double v=0;for(double a:d)', 'perf::Timer density_timer(perf::density_time); double v=0;for(double a:d)')
if '--depth' in sys.argv:
    variant+='-depth'
    src=src.replace('inline double profile_time=0;', 'inline unsigned max_depth=0; inline double profile_time=0;')
    src=src.replace('double lo,hi,flo,fhi;};', 'double lo,hi,flo,fhi; unsigned depth=0;};')
    src=src.replace('stack.pop_back();', 'stack.pop_back(); perf::max_depth=std::max(perf::max_depth,q.depth);')
    src=src.replace('q.lo,mid,q.flo,f.logLik}', 'q.lo,mid,q.flo,f.logLik,q.depth+1}')
    src=src.replace('mid,q.hi,f.logLik,q.fhi}', 'mid,q.hi,f.logLik,q.fhi,q.depth+1}')
(out/(variant+'.hpp')).write_text(src)
g=json.loads((root/family/'golden.json').read_text()); rng=random.Random(137)
for name,values in [('golden500',g['data']),('golden10000',g['data']*20),('cauchy500',[math.tan(math.pi*(rng.random()-.5)) for _ in range(500)]),('cauchy10000',[math.tan(math.pi*(rng.random()-.5)) for _ in range(10000)])]:
    (out/(name+'.txt')).write_text('\n'.join(map(repr,values)))
(out/(variant+'.cpp')).write_text('''#include "VARIANT.hpp"
#include <fstream>
#include <iostream>
#include <iomanip>
int main(int argc,char** argv){
 std::ifstream in(argv[1]); std::vector<double> x; double v; while(in>>v)x.push_back(v);
 const double nan=cppunivariate::special::kNaN;
 std::cout<<std::setprecision(17);
 for(int mask=0;mask<4;++mask){
  if(argc>2 && mask!=std::stoi(argv[2]))continue;
  perf::profiles=perf::scores=perf::observations=perf::cells=perf::residuals=0;perf::profile_time=perf::scores_time=perf::density_time=0;
  auto start=std::chrono::steady_clock::now();
  auto r=cppunivariate::slash_mle(cppunivariate::span{x.data(),x.size()},1e-9,500,(mask&2)?1.1437689913361664:nan,(mask&1)?-0.010630182705389644:nan);
  double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();
  std::cout<<argv[1]<<","<<mask<<","<<seconds<<","<<int(r.status)<<","<<r.budget_exhausted<<","<<r.mu<<","<<r.sigma<<","<<r.logLik<<","<<perf::profiles<<","<<perf::scores<<","<<perf::observations<<","<<perf::cells<<","<<perf::residuals<<","<<perf::profile_time<<","<<perf::scores_time<<","<<perf::density_time<<std::endl;
 }
}
'''.replace('VARIANT',variant))
if '--depth' in sys.argv:
    p=out/(variant+'.cpp')
    p.write_text(p.read_text().replace('<<perf::density_time<<std::endl', '<<perf::density_time<<","<<perf::max_depth<<std::endl').replace('perf::profile_time=perf::scores_time=perf::density_time=0;', 'perf::profile_time=perf::scores_time=perf::density_time=0;perf::max_depth=0;'))
print(out/(variant+'.cpp'))
